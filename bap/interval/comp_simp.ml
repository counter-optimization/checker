open Core_kernel
open Bap.Std
open Graphlib.Std
open Common
open Monads.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

module Checker(N : NumericDomain) = struct
  module E = struct
    type region = Abstract_memory.Region.t
    type regions = Abstract_memory.Region.Set.t
    type valtypes = Common.cell_t
    include Abstract_memory.Make(N)
  end
  module AI = AbstractInterpreter(N)(Abstract_memory.Region)(Abstract_memory.Region.Set)(struct type t = Common.cell_t end)(E)
  module I = Wrapping_interval
  module SS = Set.Make_binable_using_comparator(String)

  type warn = Alert.Set.t
  type t = warn

  module State = struct
    type t = { warns: warn;
               env: E.t;
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
    | None -> failwith "Couldn't extract interval information out of product domain"

  let dont_care_vars = ["ZF"; "OF"; "CF"; "AF"; "PF"; "SF"]
                       |> SS.of_list

  
  let empty : warn = Alert.Set.empty
  let join = Alert.Set.union

  let could_be_special (special_for_bw : I.t -> I.t) (to_check : I.t) : bool =
    if I.equal I.bot to_check
    then false
    else
      let special = special_for_bw to_check in
      I.contains special to_check

  let check_binop_operands (left_specials, right_specials) left right : warn =
    let left = get_intvl left in
    let right = get_intvl right in
    let fold_checker ~is_left =
      let operand = if is_left then left else right in
      let check_operand result_acc special_for_bw =
        match could_be_special special_for_bw operand with
        | true ->
           let special_str = I.to_string @@ special_for_bw operand in
           let warn = Format.sprintf (if is_left then "binop_left_%s" else "binop_right_%s") special_str in
           SS.add result_acc warn
        | false -> result_acc
      in
      check_operand
    in
    let left_res = List.fold left_specials ~init:empty ~f:(fold_checker ~is_left:true) in
    let right_res = List.fold right_specials ~init:empty ~f:(fold_checker ~is_left:false) in
    join left_res right_res

  let specials_of_binop (op : binop) : (I.t -> I.t) list * (I.t -> I.t) list =
    let one i = I.of_int ~width:(I.bitwidth i) 1 in
    let zero i = I.of_int ~width:(I.bitwidth i) 0 in
    let all_ones i =
      let bw = I.bitwidth i in 
      let uint_max_for_i = (Int.pow 2 bw) - 1 in
      I.of_int ~width:bw uint_max_for_i
    in
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

  let check_binop (op : binop) : N.t -> N.t -> warn =
    check_binop_operands (specials_of_binop op)

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
       ST.gets @@ fun st -> E.load_of_bil_exp e offs st.env
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
       let warnings = check_binop op x' y' in
       ST.get () >>= fun st ->
       ST.put { st with warns = join st.warns warnings } >>= fun _ ->
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

  let check_def (d : def term)
        (live : Live_variables.t)
        (env : E.t)
      : warn =
    let tid = Term.tid d in
    let tid_string = Tid.to_string tid in
    let lhs = Def.lhs d in
    let lhs_var_name = Var.name lhs in
    if SS.mem dont_care_vars lhs_var_name
    then empty
    else
      let init_state = State.init env tid live in
      let rhs = Def.rhs d in
      let _, final_state = ST.run (check_exp rhs) init_state in
      final_state.warns
  
  let check_elt (e : Blk.elt) (live : Live_variables.t) (env : E.t) : warn =
    match e with
    | `Def d -> check_def d live env
    | _ -> SS.empty
end

let run (module Domain : NumericDomain)
      (edges: Edge_builder.edges)
      (analysis_results : Solution.t)
      (liveness : Live_variables.t)
      (tidmap : Tid_map.t)
    : warn =
  let module Checker = Checker(Domain) in
  let analyze_edge (e : Edge_builder.edge) : warn =
    let from_tid = Edge_builder.from_ e in
    let to_tid = Edge_builder.to_ e in
    let in_state = Solution.get analysis_results from_tid in
    let insn = match Tid_map.find tidmap to_tid with
      | Some elt -> elt
      | None ->
         let tid_str = Tid.to_string to_tid in
         failwith @@
           sprintf "In Comp_simp.run, couldn't find tid %s" tid_str
    in
    Checker.check_elt insn liveness in_state
  in
  List.fold edges
    ~init:Alert.Set.empty
    ~f:(fun alerts edge ->
      let alerts' = analyze_edge edge in
      Alert.Set.union alerts alerts')

let print_results : warn -> unit =
  Alert.Set.iter ~f:(fun alert ->
      printf "Comp_simp.Checker result: %s\n" @@ Alert.to_string alert)
