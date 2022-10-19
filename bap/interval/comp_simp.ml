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

  let add_ident = N.of_int 0
  let mul_ident = N.of_int 1
  let mul_zero = N.of_int 0

  let could_be_special (special : N.t) (to_check : N.t) : bool =
    N.contains special to_check

  let check_binop_operands (left_specials, right_specials) left right : t =
    let fold_checker is_left =
      let operand = if is_left then left else right in
      let warn_format = if is_left then "binop_left_%s" else "binop_right_%s" in
      fun acc spec ->
      if could_be_special spec operand
      then
        begin
          let special_str = N.to_string spec in
          let warn = Format.sprintf (if is_left then "binop_left_%s" else "binop_right_%s") special_str in
          SS.add acc warn
        end
      else acc
    in
    let left_res = List.fold left_specials ~init:SS.empty ~f:(fold_checker true) in
    let right_res = List.fold right_specials ~init:SS.empty ~f:(fold_checker false) in
    SS.union left_res right_res

  let check_unop_operand specials operand : t =
    let fold_checker acc spec =
      if could_be_special spec operand
      then
        begin
          let special_str = N.to_string spec in
          let warn = Format.sprintf "unop_%s" special_str in
          SS.add acc warn
        end
      else
        acc
    in
    List.fold specials ~init:SS.empty ~f:fold_checker

  let specials_of_binop (op : binop) : N.t list * N.t list =
    match op with
    | Bil.PLUS -> [add_ident], [add_ident]
    | Bil.MINUS -> [], [add_ident]
    | Bil.TIMES -> [mul_zero; mul_ident], [mul_zero; mul_ident]
    | Bil.DIVIDE -> [], []
    | Bil.SDIVIDE -> [], []
    | Bil.MOD -> [], []
    | Bil.SMOD -> [], []
    | Bil.LSHIFT -> [], []
    | Bil.RSHIFT -> [], []
    | Bil.ARSHIFT -> [], []
    | Bil.AND -> [], []
    | Bil.OR -> [], []
    | Bil.XOR -> [], []
    | Bil.EQ -> [], []
    | Bil.NEQ -> [], []
    | Bil.LT -> [], []
    | Bil.LE -> [], []
    | Bil.SLT -> [], []
    | Bil.SLE -> [], []

  let specials_of_unop (op : unop) : N.t list =
    match op with
    | Bil.NEG -> []
    | Bil.NOT -> []

  let check_binop (op : binop) : N.t -> N.t -> t =
    check_binop_operands (specials_of_binop op)

  let check_unop (op : unop) : N.t -> t =
    check_unop_operand (specials_of_unop op)

  (* TODO: don't flag on constants *)
  let rec check_exp (e : Bil.exp) (env : env) : expr_t =
    match e with
    | Bil.BinOp (op, x, y) ->
       let (x', resx) = check_exp x env in
       let (y', resy) = check_exp y env in
       let checker_res = check_binop op x' y' in
       let final_val = (AI.denote_binop op) x' y' in
       (final_val, SS.union resx resy |> SS.union checker_res)
    | Bil.UnOp (op, x) ->
       let (x', resx) = check_exp x env in
       let checker_res = check_unop op x' in
       let final_val = (AI.denote_unop op) x' in
       (final_val, SS.union checker_res resx)
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
