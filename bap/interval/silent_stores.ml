open Core
open Bap.Std
open Graphlib.Std
open Common
open Monads.Std

module Checker(N : NumericDomain) = struct
  
  module Env = struct
    type region = Common.Region.t
    type regions = Common.Region.Set.t
    type valtypes = Common.cell_t
    include Abstract_memory.Make(N)
  end

  module AI = AbstractInterpreter(N)(Common.Region)(Common.Region.Set)(struct type t = Common.cell_t end)(Env)
  
  module I = Wrapping_interval

  module Taint = Checker_taint.Analysis

  (* string set *)
  module SS = Set.Make_binable_using_comparator(String)
  
  type warns = Alert.Set.t
  type t = warns

  let name = "silent-stores"

  module State = struct
    type t = { warns: warns;
               env: Env.t;
               tid : Tid.t;
               sub : sub term;
               proj : project;
               do_symex : bool;
               estats : Common.EvalStats.t;
               symex_state : SymExChecker.state;
               liveness : Live_variables.t } 

    let init in_state tid liveness sub dep_bound do_symex proj =
      { warns = Alert.Set.empty;
        env = in_state;
        tid = tid;
        proj;
        do_symex;
        estats = EvalStats.init;
        sub;
        symex_state = { SymExChecker.default_state
                        with dep_bound };
        liveness = liveness }
  end
    
  module ST = struct
    include Monad.State.T1(State)(Monad.Ident)
    include Monad.State.Make(State)(Monad.Ident) 
  end
  open ST.Syntax

  let update_eval_stats updater : unit ST.t =
    ST.update @@ fun st ->
    { st with estats = updater st.estats }

  type state = State.t

  let dep_bound = 8

  let get_intvl : N.t -> Wrapping_interval.t =
    match N.get Wrapping_interval.key with
    | Some f -> f
    | None -> failwith "Couldn't extract interval information out of product domain in silent store checker"

  let get_taint : N.t -> Taint.t =
    match N.get Taint.key with
    | Some f -> f
    | None -> failwith "Couldn't extract taint information out of product domain in silent store checker"

  let is_tainted : N.t -> bool =
    fun n -> Taint.is_tainted @@ get_taint n

  let dont_care_vars = ["ZF"; "OF"; "CF"; "AF"; "PF"; "SF"]
                       |> SS.of_list

  let empty : warns = Alert.Set.empty
  let join : warns -> warns -> warns = Alert.Set.union

  let tids_to_def_terms tids (s : state) : def term list option =
    let blks = Term.enum blk_t s.sub in
    let defs = Seq.map blks ~f:(Term.enum def_t)
               |> Seq.join in
    let select_set = Set.of_list (module Tid) tids in
    let selected_defs = Seq.filter defs
                          ~f:(fun dt -> Set.mem select_set @@ Term.tid dt) in
    let selected_defs = Seq.to_list selected_defs in
    if List.length selected_defs = List.length tids
    then Some selected_defs
    else None

  let print_dep_defs tid dts =
    List.iter dts
      ~f:(fun dt ->
        printf "dep def of tid %a :: %a -- %a\n%!"
          Tid.ppo tid
          Tid.ppo (Term.tid dt)
          Def.ppo dt)

  let get_dependent_defs : def term list option ST.t =
    ST.gets @@ fun s ->
    let dependents = Live_variables.dependents_up_to_bound s.tid s.symex_state.dep_bound s.liveness in
    let dep_defs = tids_to_def_terms dependents s in
    Option.(>>|) dep_defs @@ fun dep_defs ->
    (* check that the potentially leaky def is in the dependent defs list and last *)
    let () = match List.findi dep_defs ~f:(fun _ dt -> Tid.equal (Term.tid dt) s.tid) with
      | Some (pos, dt) -> if pos <> (List.length dep_defs - 1)
                          then
                            let () = print_dep_defs s.tid dep_defs in
                            failwith @@ sprintf "in computing dependent defs, target tid %a not last in dep defs: %s"
                                          Tid.pps s.tid (List.to_string ~f:(Def.to_string) dep_defs)
                          else ()
      | None ->
         let () = print_dep_defs s.tid dep_defs in
         failwith @@ sprintf "in computing dependent defs, target tid %a not in dep defs: %s"
                       Tid.pps s.tid (List.to_string ~f:(Def.to_string) dep_defs)
    in
    dep_defs

  (* TODO: don't flag on constants *)
  (* cases:
   * 1) if there has never been a store to this location and
   *    the new value or the idx is not secret, do not alert
   *)
  let rec check_exp (e : Bil.exp) : N.t ST.t =
    let eval_in_ai (e : Bil.exp) (st : State.t) : N.t ST.t =
      let exp_evaler = AI.denote_exp e in
      let (res, _) = AI.ST.run exp_evaler st.env in
      ST.return res
    in
    match e with
    | Bil.Load (_mem, idx, _endian, size) ->
       check_exp idx >>= fun offs ->
       ST.gets @@ fun st ->
                  (match Env.load_of_bil_exp e offs size st.env with
                   | Ok (v, _) -> v
                   | Error e -> failwith @@ Error.to_string_hum e)
    | Bil.Store (mem, idx, v, endian, size) ->
       update_eval_stats EvalStats.incr_total_considered >>= fun () ->
       ST.get () >>= fun st ->
       check_exp idx >>= fun idx_val ->
       let load_of_old_val = Bil.Load (mem, idx, endian, size) in
       let eval_loaded_val = Env.load_of_bil_exp load_of_old_val idx_val size st.env in
       let old_val = match eval_loaded_val with
         | Ok (old_val, _) -> old_val
         | Error e ->
            let () = printf "silentstores.check_exp: %s\n" @@
                       Error.to_string_hum e in
            N.top in
       check_exp v >>= fun new_val ->
       let old_tainted = is_tainted old_val in
       let new_tainted = is_tainted new_val in
       (* let idx_tainted = is_tainted idx_val in *)
       if old_tainted || new_tainted (* || idx_tainted *)
       then
         begin
           get_dependent_defs >>= fun deps ->
           ST.get () >>= fun st ->
           let can_do_last_symex_check = st.do_symex && Option.is_some deps in
           if can_do_last_symex_check
           then
             let deps = Option.value_exn deps in
             let do_check = Symbolic.Executor.eval_def_list deps in
             let init_st = Symbolic.Executor.init ~do_ss:true deps st.tid in
             let (), fini_st = Symbolic.Executor.run do_check init_st in
             let failed_ss = fini_st.failed_ss in
             let () = Format.printf "Last ditch symex failed ss? %B\n%!" failed_ss in
             if failed_ss
             then
               let alert_desc = "Silent stores failed last ditch sym ex check" in
               let alert : Alert.t = { tid = st.tid;
                                       opcode = None;
                                       addr = None;
                                       rpo_idx = None;
                                       sub_name = None;
                                       flags_live = SS.empty;
                                       is_live = None;
                                       reason = Alert.SilentStores;
                                       desc = alert_desc;
                                       left_val = None;
                                       right_val = None; 
                                       problematic_operands = None }
               in
               ST.put { st with warns = Alert.Set.add st.warns alert } >>= fun () ->
               ST.return N.bot
             else
               update_eval_stats EvalStats.incr_symex_pruned >>= fun () ->
               ST.return N.bot
           else
             let alert_desc = "Store of val, prev val, or mem idx is tainted" in
             let alert : Alert.t = { tid = st.tid;
                                     opcode = None;
                                     addr = None;
                                     rpo_idx = None;
                                     sub_name = None;
                                     flags_live = SS.empty;
                                     is_live = None;
                                     reason = Alert.SilentStores;
                                     desc = alert_desc;
                                     left_val = None;
                                     right_val = None;
                                     problematic_operands = None } in
             ST.put { st with warns = Alert.Set.add st.warns alert } >>= fun () ->
             ST.return N.bot
         end
       else
         update_eval_stats EvalStats.incr_taint_pruned >>= fun () ->
         ST.return N.bot
    | Bil.Var _
      | Bil.Int _
      | Bil.Unknown _->
       ST.get () >>= fun st ->
       eval_in_ai e st
    | Bil.BinOp (op, x, y) ->
       check_exp x >>= fun x' ->
       check_exp  y >>= fun y' ->
       ST.get () >>= fun st ->
       eval_in_ai e st 
    | Bil.UnOp (op, x) ->
       check_exp x >>= fun x' ->
       ST.return @@ AI.denote_unop op x'
    | Bil.Cast (cast, n, exp) ->
       check_exp exp >>= fun exp' ->
       ST.return @@ AI.denote_cast cast n exp'
    | Bil.Ite (cond, ifthen, ifelse) ->
       check_exp cond >>= fun cond' ->
       let truthy = N.could_be_true cond' in
       let falsy = N.could_be_false cond' in
       if truthy && not falsy
       then check_exp ifthen
       else
         if not truthy && falsy
         then check_exp ifelse
         else
           check_exp ifthen >>= fun then' ->
           check_exp ifelse >>= fun else' ->
           ST.return @@ N.join then' else'
    | Bil.Let (v, e, b) ->
       check_exp e >>= fun e' ->
       let name = Var.name v in
       ST.get () >>= fun st ->
       let env' = Env.set name e' st.env in
       ST.put { st with env = env' } >>= fun _ ->
       check_exp b >>= fun body_res ->
       ST.put st >>= fun _ ->
       ST.return body_res
    | Bil.Extract (hi, lo, e) ->
       check_exp e >>= fun e' ->
       ST.return @@ N.extract e' hi lo
    | Bil.Concat (x, y) ->
       check_exp x >>= fun x' ->
       check_exp y >>= fun y' ->
       ST.return @@ N.concat x' y'

  let check_def (d : def term)
        (live : Live_variables.t)
        (env : Env.t)
        (sub : sub term)
        do_symex
        proj
      : warns =
    let tid = Term.tid d in
    let lhs = Def.lhs d in
    let lhs_var_name = Var.name lhs in
    if SS.mem dont_care_vars lhs_var_name
    then empty
    else
      let init_state = State.init env tid live sub dep_bound do_symex proj in
      let rhs = Def.rhs d in
      let _, final_state = ST.run (check_exp rhs) init_state in
      final_state.warns
  
  let check_elt (e : Blk.elt) (live : Live_variables.t) (env : Env.t) (sub : sub term) proj do_symex : warns =
    match e with
    | `Def d -> check_def d live env sub do_symex proj
    | _ -> empty
end
