open Core
open Bap.Std
open Graphlib.Std
open Common
open Monads.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

module Checker(N : NumericDomain) = struct
  module E = Abstract_memory.Make(N)
  module AI = AbstractInterpreter(N)(E)
  module I = Wrapping_interval
  module SS = Set.Make_binable_using_comparator(String)

  type t = SS.t
  type expr_t = N.t * SS.t
  type env = E.t

  let get_intvl : N.t -> Wrapping_interval.t =
    match N.get Wrapping_interval.key with
    | Some f -> f
    | None -> failwith "Couldn't extract interval information out of product domain"

  let dont_care_vars = ["ZF"; "OF"; "CF"; "AF"; "PF"; "SF"]

  let print_results : t -> unit = SS.iter ~f:(Format.printf "%s\n%!")
  let to_string (res : t) : string =
    String.concat ~sep:" " @@ SS.to_list res 
  let empty : t = SS.empty
  let join = SS.union

  let could_be_special (special_for_bw : I.t -> I.t) (to_check : I.t) : bool =
    if I.equal I.bot to_check
    then false
    else
      let special = special_for_bw to_check in
      I.contains special to_check

  let check_binop_operands (left_specials, right_specials) left right : t =
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

  let check_binop (op : binop) : N.t -> N.t -> t =
    check_binop_operands (specials_of_binop op)

  (* TODO: don't flag on constants *)
  let rec check_exp (e : Bil.exp) (env : env) : expr_t =
    match e with
    | Bil.Load _
      | Bil.Store _
      | Bil.Var _
      | Bil.Int _
      | Bil.Unknown _->
       let (e', env) = AI.denote_exp e env in
       (, empty)
    | Bil.BinOp (op, x, y) ->
       let (x', resx) = check_exp x env in
       let (y', resy) = check_exp y env in
       let checker_res = check_binop op x' y' in
       (AI.denote_exp e env, join (join resx resy) checker_res)
    | Bil.UnOp (op, x) ->
       let (x', resx) = check_exp x env in
       ((AI.denote_unop op) x', resx)
    | Bil.Cast (cast, n, exp) ->
       let (x', resx) = check_exp exp env in
       (AI.denote_exp e env, resx)
    | Bil.Ite (c, t, e) ->
       let (t', rest) = check_exp t env in
       let (e', rese) = check_exp e env in
       (AI.denote_exp e env, join rest rese)
    | Bil.Let (v, e, b) ->
       let (e', rese) = check_exp e env in
       let (b', resb) = check_exp b env in
       (AI.denote_exp e env, join rese resb)
    | Bil.Extract (h, l, e) ->
       let (e', rese) = check_exp e env in
       (AI.denote_exp e env, rese)
    | Bil.Concat (x, y) ->
       let (x', resx) = check_exp x env in
       let (y', resy) = check_exp y env in
       (AI.denote_exp e env, join resx resy)

  let check_def (d : def term) (env : env) : t =
    let tid = Term.tid d in
    let lhs = Def.lhs d in
    let lhs_var_name = Var.name lhs in
    if List.mem dont_care_vars lhs_var_name ~equal:String.equal
    then SS.empty
    else
      begin
        let rhs = Def.rhs d in
        let (_ , checker_result) = check_exp rhs env in
        SS.map checker_result ~f:(fun warn_str ->
            Format.sprintf "%s::%s" (Tid.to_string tid) warn_str)
      end

  let check_elt (e : Blk.elt) (env : env) : t =
    match e with
    | `Def d -> check_def d env
    | _ -> SS.empty
end
