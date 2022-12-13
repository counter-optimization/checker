open Core_kernel
open Bap.Std
open Graphlib.Std
open Common
open Monads.Std

module Checker(N : NumericDomain) = struct
  
  module Env = struct
    type region = Abstract_memory.Region.t
    type regions = Abstract_memory.Region.Set.t
    type valtypes = Common.cell_t
    include Abstract_memory.Make(N)
  end

  module AI = AbstractInterpreter(N)(Abstract_memory.Region)(Abstract_memory.Region.Set)(struct type t = Common.cell_t end)(Env)
  
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
               liveness : Live_variables.t } 

    let init in_state tid liveness =
      { warns = Alert.Set.empty;
        env = in_state;
        tid = tid;
        liveness = liveness }
  end
    
  module ST = struct
    include Monad.State.T1(State)(Monad.Ident)
    include Monad.State.Make(State)(Monad.Ident) 
  end
  open ST.Syntax

  let get_intvl : N.t -> Wrapping_interval.t =
    match N.get Wrapping_interval.key with
    | Some f -> f
    | None -> failwith "Couldn't extract interval information out of product domain in silent store checker"

  let get_taint : N.t -> Taint.t =
    match N.get Taint.key with
    | Some f -> f
    | None -> failwith "Couldn't extract taint information out of product domain in silent store checker"

  let dont_care_vars = ["ZF"; "OF"; "CF"; "AF"; "PF"; "SF"]
                       |> SS.of_list

  let empty : warns = Alert.Set.empty
  let join : warns -> warns -> warns = Alert.Set.union

  (* TODO: don't flag on constants *)
  let rec check_exp (e : Bil.exp) : N.t ST.t =
    let eval_in_ai (e : Bil.exp) (st : State.t) : N.t ST.t =
      let exp_evaler = AI.denote_exp e in
      let (res, _) = AI.ST.run exp_evaler st.env in
      ST.return res
    in
    match e with
    | Bil.Load (_mem, idx, _endian, size) ->
       check_exp idx >>= fun offs ->
       ST.gets @@ fun st -> Env.load_of_bil_exp e offs st.env
    | Bil.Store (mem, idx, v, endian, size) ->
       ST.get () >>= fun st ->
       let load_of_old_val = Bil.Load (mem, idx, endian, size) in
       eval_in_ai load_of_old_val st >>= fun old_val ->
       check_exp idx >>= fun idx_val ->
       check_exp v >>= fun new_val ->
       let old_tainted = Taint.is_tainted @@ get_taint old_val in
       let new_tainted = Taint.is_tainted @@ get_taint new_val in
       let idx_tainted = Taint.is_tainted @@ get_taint idx_val in
       if old_tainted || new_tainted || idx_tainted
       then
         begin
           let alert_desc = "Store of val, prev val, or mem idx is tainted" in
           let alert : Alert.t = { tid = st.tid;
                                   flags_live = None; (* todo *)
                                   reason = Alert.SilentStores;
                                   desc = alert_desc;
                                   left_val = None;
                                   right_val = None;
                                   problematic_operands = None }
           in
           ST.put { st with warns = Alert.Set.add st.warns alert } >>= fun () ->
           ST.return N.bot
         end
       else
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
      : warns =
    let tid = Term.tid d in
    let lhs = Def.lhs d in
    let lhs_var_name = Var.name lhs in
    if SS.mem dont_care_vars lhs_var_name
    then empty
    else
      let init_state = State.init env tid live in
      let rhs = Def.rhs d in
      let _, final_state = ST.run (check_exp rhs) init_state in
      final_state.warns
  
  let check_elt (e : Blk.elt) (live : Live_variables.t) (env : Env.t) : warns =
    match e with
    | `Def d -> check_def d live env
    | _ -> empty
end
