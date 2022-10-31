open Core_kernel
open Bap.Std
open Graphlib.Std
open Common

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

module Checker(N : NumericDomain) = struct
  module E = NumericEnv(N)
  module AI = AbstractInterpreter(N)
  module SS = Set.Make_binable_using_comparator(String)

  type t = SS.t
  type expr_t = N.t * SS.t
  type env = E.t

  let print_results : t -> unit = SS.iter ~f:(Format.printf "%s\n%!")
  let init_results : t = SS.empty
  let join_results = SS.union

  let could_be_special (special : N.t -> N.t) (to_check : N.t) : bool =
    let special_of_to_check = special to_check in
    N.contains special_of_to_check to_check

  let check_binop_operands (left_specials, right_specials) left right : t =
    let fold_checker ~is_left =
      let operand = if is_left then left else right in
      fun acc spec ->
      if could_be_special spec operand
      then
        begin
          let special_str = N.to_string @@ spec operand in
          let warn = Format.sprintf (if is_left then "binop_left_%s" else "binop_right_%s") special_str in
          SS.add acc warn
        end
      else acc
    in
    let left_res = List.fold left_specials ~init:SS.empty ~f:(fold_checker ~is_left:true) in
    let right_res = List.fold right_specials ~init:SS.empty ~f:(fold_checker ~is_left:false) in
    SS.union left_res right_res

  let specials_of_binop (op : binop) : (N.t -> N.t) list * (N.t -> N.t) list =
    let one i = N.of_int ~width:(N.bitwidth i) 1 in
    let zero i = N.of_int ~width:(N.bitwidth i) 0 in
    let all_ones i =
      let bw = N.bitwidth i in 
      let uint_max_for_i = (Int.pow 2 bw) - 1 in
      N.of_int ~width:bw uint_max_for_i
    in
    let pow_two =
      let two = Z.of_int 2 in
      let potential_expts = List.init 63 ~f:succ in
      List.map potential_expts ~f:(fun pot_expt ->
          (fun i ->
            let bw = N.bitwidth i in
            let expt = if bw < pot_expt
                       then bw
                       else pot_expt
            in
            N.of_z ~width:bw @@ Z.pow two expt))
    in
    let onel = [one] in
    let zerol = [zero] in
    let zeroallonesl = [zero; all_ones] in
    let onezerol = [one; zero] in
    match op with
    | Bil.PLUS -> zerol, zerol
    | Bil.MINUS -> [], zerol
    | Bil.TIMES -> onezerol, List.append onezerol pow_two
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
    | Bil.BinOp (op, x, y) ->
       let (x', resx) = check_exp x env in
       let (y', resy) = check_exp y env in
       let checker_res = check_binop op x' y' in
       let final_val = (AI.denote_binop op) x' y' in
       (final_val, SS.union resx resy |> SS.union checker_res)
    | _ -> (AI.denote_exp e env, SS.empty)

  let check_def (d : def term) (env : env) : t =
    let tid = Term.tid d in
    let rhs = Def.rhs d in
    let (_ , checker_result) = check_exp rhs env in
    SS.map checker_result ~f:(fun warn_str ->
        Format.sprintf "%s::%s" (Tid.to_string tid) warn_str)

  let check_elt (e : Blk.elt) (env : env) : t =
    match e with
    | `Def d -> check_def d env
    | _ -> SS.empty
end
