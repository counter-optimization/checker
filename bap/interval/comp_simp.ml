open Core
open Bap.Std
open Graphlib.Std
open Common
open Bap_primus.Std
open Monads.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

module ABI = Common.ABI

module Mxtor = struct
    (* need to write primus components to:
       1) set the machine entry point to the mock sub
       2) set up init state with all mock blk free vars havocd
       3) SS component: on store, do the silent store checking
       4) CS component: on binop, add the appropriate assertion 

       other notes:
       - only the last def needs to be checked, but which args?
         => will need to bring the taint information in also
     *)
  let bap_package = "bap"
  
  let symex_system_name = "symbolic-executor"
  
  (* let symex_system = Primus.System.Repository.get *)
  (*                      ~package:bap_package *)
  (*                      symex_system_name *)

  module Machine = Primus.Analysis
  module Linker = Primus.Linker.Make(Machine)

  module CompSimpSymExChecker(Machine : Primus.Machine.S) = struct
    open Machine.Syntax

    module Linker = Primus.Linker.Make(Machine)

    let on_enter_sub sub =
      Machine.return @@ printf "CompSimpSymExChecker visited sub %s\n%!"
                          (Sub.name sub)

    let on_start start_str =
      Machine.return @@
        printf "CompSimpSymExChecker on_start: %s\n%!" start_str

    
    let init () =
      Machine.sequence [
          Primus.Interpreter.enter_sub >>> on_enter_sub;
          Primus.System.start >>> on_start
        ]
  end

  let () = Primus.Components.register_generic
             ~package:Common.package
             ~desc:"comp simp sym checker component"
             "cs-symex-checker"
             (module CompSimpSymExChecker)

  let cs_system = Primus.System.define
                    ~desc:"Comp Simp sym ex last resort checker"
                    ~depends_on:[
                      Primus.System.depends_on
                        ~package:bap_package
                        symex_system_name
                    ]
                    ~components:[
                      Primus.System.component
                        ~package:Common.package
                        "cs-symex-checker"
                    ]
                    ~package:Common.package
                    "comp-simp-sym-ex-checker-sys"
  

  module SilentStoreSymExChecker= struct
  end
end

module Checker(N : NumericDomain) = struct
  
  module E = struct
    type region = Common.Region.t
    type regions = Common.Region.Set.t
    type valtypes = Common.cell_t
    include Abstract_memory.Make(N)
  end
  
  module AI = AbstractInterpreter(N)(Common.Region)(Common.Region.Set)(struct type t = Common.cell_t end)(E)
  
  module I = Wrapping_interval
  
  module SS = Common.SS

  type warns = Alert.Set.t
  
  type t = warns

  let name = "comp-simp"

  module State = struct
    type t = {
        warns: warns;
        env: E.t;
        tid : Tid.t;
        liveness : Live_variables.t;
        symex_state : SymExChecker.state;
        sub : sub term;
        proj : Project.t
      } 

    let init in_state tid liveness sub dep_bound proj : t =
      { warns = Alert.Set.empty;
        env = in_state;
        tid = tid;
        liveness = liveness;
        symex_state = SymExChecker.default_state;
        proj;
        sub }
  end

  module ST = struct
    include Monad.State.T1(State)(Monad.Ident)
    include Monad.State.Make(State)(Monad.Ident) 
  end
  open ST.Syntax

  type state = State.t

  

  (* todo, make an optional CL arg *)
  let dep_bound = 10

  let get_intvl : N.t -> Wrapping_interval.t =
    match N.get Wrapping_interval.key with
    | Some f -> f
    | None -> failwith "Couldn't extract interval information out of product domain in comp simp checker"

  let get_taint : N.t -> Checker_taint.Analysis.t =
    match N.get Checker_taint.Analysis.key with
    | Some f -> f
    | None -> failwith "Couldn't extract taint information out of product domain in comp simp checker"

  let dont_care_vars = ABI.flag_names (* don't comp simp check flag calculations *)
  
  let empty : warns = Alert.Set.empty
  
  let join : warns -> warns -> warns = Alert.Set.union

  let could_be_special (special_for_bw : I.t -> I.t) (to_check : I.t) : bool =
    if I.equal I.bot to_check
    then false
    else
      let special = special_for_bw to_check in
      I.contains special to_check

  (* if the list of def terms found in the non-ssa sub term 
     differs from the list from the liveness analysis's ssa sub term,
     then the difference most likely comes from an interproc call
     or a phi node. for now, the simple symex checker doesn't handle
     these things, only straight line assignments, so return None 
     and only use the interval analysis results for reporting here. *)
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
    (* let () = printf "Dependents for tid %a are:\n%!" Tid.ppo s.tid in *)
    (* let () = List.iter dependents ~f:(printf "\t%a\n%!" Tid.ppo) in *)
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
    (* let () = List.iter dep_defs ~f:(fun d -> printf "%a :: %a\n%!" Tid.ppo (Term.tid d) Def.ppo d) in *)
    dep_defs

  let build_mock_sub_for_mx (defs : def term list) : sub term ST.t =
    let blk = Blk.create ~defs () in
    let free_vars = Blk.free_vars blk in
    ST.update @@ (fun st ->
      { st with symex_state = { st.symex_state with mock_free_vars = free_vars } })
    >>= fun () ->
    ST.gets @@ fun st ->
    Sub.create ~blks:[blk] ~name:st.symex_state.mock_sub_name ()
    
  let check_binop_operands (specials : (I.t -> I.t) list * (I.t -> I.t) list)
                           (op : binop) ~(check_left : bool) ~(check_right : bool)
                           (left : N.t) (right : N.t) : unit ST.t =
    let left_specials = fst specials in
    let right_specials = snd specials in
    let left = get_intvl left in
    let right = get_intvl right in
    let fold_checker ~is_left =
      let operand = if is_left then left else right in
      let problematic_operand_indice = if is_left then 0 else 1 in
      let check_operand result_acc special_for_bw =
        if could_be_special special_for_bw operand
        then
          result_acc >>= fun () ->
          ST.get () >>= fun st ->
          get_dependent_defs >>= fun deps ->
          if Option.is_some deps
          then
            let deps = Option.value_exn deps in
            build_mock_sub_for_mx deps >>= fun mocksub ->
            let mocksub_name = `tid (Term.tid mocksub) in
            let kb_state = Toplevel.current () in
            let () = printf "starting microexecution...%!" in
            let start = Mxtor.Linker.exec mocksub_name in
            let res = Primus.System.run Mxtor.cs_system st.proj kb_state ~start in
            let () = printf "done\n%!" in
            let () = match res with
              | Ok (exit_status, proj', kb_state') -> ()
              | Error e -> failwith @@ Bap_knowledge.Knowledge.Conflict.to_string e in
            let binop_str = Common.binop_to_string op in
            let left_str = if is_left
                           then Some (I.to_string operand)
                           else None in
            let right_str = if not is_left
                            then Some (I.to_string operand)
                            else None in
            let alert : Alert.t = { tid = st.tid;
                                    opcode = None;
                                    addr = None;
                                    rpo_idx = None;
                                    sub_name = None;
                                    flags_live = SS.empty;
                                    is_live = None;
                                    reason = Alert.CompSimp;
                                    desc = binop_str;
                                    left_val = left_str;
                                    right_val = right_str;
                                    problematic_operands = Some [problematic_operand_indice] } in
            ST.update @@ fun old_st ->
                         { old_st with warns = Alert.Set.add old_st.warns alert }
          else
            let binop_str = Common.binop_to_string op in
            let left_str = if is_left
                           then Some (I.to_string operand)
                           else None in
            let right_str = if not is_left
                            then Some (I.to_string operand)
                            else None in
            let alert : Alert.t = { tid = st.tid;
                                    opcode = None;
                                    addr = None;
                                    rpo_idx = None;
                                    sub_name = None;
                                    flags_live = SS.empty;
                                    is_live = None;
                                    reason = Alert.CompSimp;
                                    desc = binop_str;
                                    left_val = left_str;
                                    right_val = right_str;
                                    problematic_operands = Some [problematic_operand_indice] } in
            ST.update @@ fun old_st ->
                         { old_st with warns = Alert.Set.add old_st.warns alert }
        else
          result_acc in
      check_operand in
    (if check_left
    then List.fold left_specials
           ~init:(ST.return ())
           ~f:(fold_checker ~is_left:true)
     else ST.return ())
    >>= fun () ->
    if check_right
    then List.fold right_specials
           ~init:(ST.return ())
           ~f:(fold_checker ~is_left:false)
    else ST.return ()

  let specials_of_binop (op : binop) : (I.t -> I.t) list * (I.t -> I.t) list =
    let one i = I.of_int ~width:(I.bitwidth i) 1 in
    let zero i = I.of_int ~width:(I.bitwidth i) 0 in
    let all_ones i =
      let bw = I.bitwidth i in
      let ones = Word.ones bw in
      Wrapping_interval.of_word ones in
    let onel = [one] in
    let zerol = [zero] in
    let zeroallonesl = [zero; all_ones] in
    let onezerol = [one; zero] in
    match op with
    | Bil.PLUS -> zerol, zerol
    | Bil.MINUS -> [], zerol
    | Bil.TIMES -> onezerol, onezerol
    | Bil.DIVIDE | Bil.SDIVIDE -> zerol, onel
    | Bil.LSHIFT | Bil.RSHIFT -> zerol, zerol
    | Bil.ARSHIFT -> zeroallonesl, zerol
    | Bil.AND -> zeroallonesl, zeroallonesl
    | Bil.OR -> zeroallonesl, zeroallonesl
    | Bil.XOR -> zerol, zerol
    | Bil.MOD -> [], []
    | Bil.SMOD -> [], []
    | Bil.EQ -> [], []
    | Bil.NEQ -> [], []
    | Bil.LT -> [], []
    | Bil.LE -> [], []
    | Bil.SLT -> [], []
    | Bil.SLE -> [], []

  let check_binop (op : binop) ~(check_left : bool) ~(check_right : bool)
      : N.t -> N.t -> unit ST.t =
    let specials = specials_of_binop op in
    let checker = check_binop_operands specials op ~check_left ~check_right in
    checker

  let is_tainted (exp : N.t) : bool =
    match get_taint exp with
    | Taint -> true
    | Notaint -> false

  let rec is_const (exp : Bil.exp) : bool =
    match exp with
     | Bil.Load (_, _, _, _) -> false
     | Bil.Store (_, _, _, _, _) -> false
     | Bil.BinOp (_, left, right) -> is_const left && is_const right
     | Bil.UnOp (_, subexp) -> is_const subexp
     | Bil.Var _ -> false
     | Bil.Int _ -> true
     | Bil.Cast (_, _, subexp) -> is_const subexp
     | Bil.Let (_, _, body) -> is_const body
     | Bil.Unknown (_, _) -> false
     | Bil.Ite (_, then', else') -> is_const then' && is_const else'
     | Bil.Extract (_, _, subexp) -> is_const subexp
     | Bil.Concat (left, right) -> is_const left && is_const right

  let rec check_exp (e : Bil.exp) : N.t ST.t =
    let eval_in_ai (e : Bil.exp) (st : State.t) : N.t ST.t =
      let exp_evaler = AI.denote_exp e in
      let (res, _) = AI.ST.run exp_evaler st.env in
      ST.return res in
    match e with
    | Bil.Load (_mem, idx, _endian, size) ->
       check_exp idx >>= fun offs ->
       ST.gets @@ fun st ->
                  (match E.load_of_bil_exp e offs size st.env with
                  | Ok (v, _) -> v
                  | Error e -> failwith @@ Error.to_string_hum e)
    | Bil.Store (_mem, idx, v, _endian, size) ->
       check_exp idx >>= fun offs ->
       check_exp v 
    | Bil.Var _
      | Bil.Int _
      | Bil.Unknown _->
       ST.get () >>= fun st ->
       eval_in_ai e st
    | Bil.BinOp (op, x, y) ->
       check_exp x >>= fun x' ->
       check_exp y >>= fun y' ->
       let should_check_left = is_tainted x' && not (is_const x) in
       let should_check_right = is_tainted y' && not (is_const y) in
       let checker = check_binop op
                                 ~check_left:should_check_left
                                 ~check_right:should_check_right in
       checker x' y' >>= fun () ->
       let binop = AI.denote_binop op in
       let expr_res = binop x' y' in
       ST.return expr_res
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
       let env' = E.set name e' st.env in
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

  (* early bail outs:
       don't comp simp check the lifted flag calculations, and
       don't comp simp check if the def is not tainted--all members of the rhs
         expression tree are untainted then *)
  let check_def (d : def term) (live : Live_variables.t) (env : E.t) (sub : sub term) proj : warns =
    let tid = Term.tid d in
    let lhs = Def.lhs d in
    let lhs_var_name = Var.name lhs in
    if ABI.var_name_is_flag lhs_var_name
    then empty
    else
      let is_tainted = is_tainted @@ E.lookup lhs_var_name env in
      if not is_tainted
      then empty
      else
        let init_state = State.init env tid live sub dep_bound proj in
        let rhs = Def.rhs d in
        let _, final_state = ST.run (check_exp rhs) init_state in
        final_state.warns
  
  let check_elt (e : Blk.elt) (live : Live_variables.t) (env : E.t) (sub : sub term) proj : warns =
    match e with
    | `Def d -> check_def d live env sub proj
    | _ -> empty
end
