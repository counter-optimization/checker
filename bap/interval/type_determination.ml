open Core
open Bap.Std
open Monads.Std

(** for determining types of variables:
    each variable's type is a fixed-width bitvector,
    so determining types comes down to figuring out each
    variable's width at it's specific def
 *)

module Typed = struct
  type t = Bitvector of int [@@deriving compare, bin_io, sexp]

  let concat (Bitvector left) (Bitvector right) = Bitvector (left + right)

  let to_string = function
    | Bitvector sz -> Format.sprintf "(bitvector %d)" sz
end

module TypedMap = Map.Make_using_comparator(String)

module T = struct
  type t = Typed.t TypedMap.t [@@deriving compare, sexp]
end

include T

type typed_var

type var = string

type typd = Typed.t

module State = struct
  type t = {
    types : T.t;
  } [@@deriving compare, sexp]

  let print : T.t -> unit =
    Map.iteri ~f:(fun ~key ~data ->
        Format.printf "Var %s has type %s\n%!" key (Typed.to_string data))
end

module ST = struct
  include Monad.State.T1(State)(Monad.Ident)
  include Monad.State.Make(State)(Monad.Ident)
end
open ST.Syntax

let get_cur_type_map : Typed.t TypedMap.t ST.t =
  ST.gets @@ fun st -> st.types

let set_cur_type_map new_map : unit ST.t =
  ST.update @@ fun st -> { types = new_map }

let has_type_in_env varname : bool ST.t =
  ST.gets @@ fun st -> TypedMap.mem st.types varname

let get_type_in_env varname : Typed.t ST.t =
  ST.gets @@ fun st -> TypedMap.find_exn st.types varname

let set_type_for_var (v : var) (t : typd) : unit ST.t =
  if String.equal v "mem"
  then failwith "Can't type memory variable in Type_determination.set_type_for_var"
  else
    get_cur_type_map >>= fun cur_type_env ->
    let updated = Map.set cur_type_env ~key:v ~data:t in
    set_cur_type_map updated

let is_abi_typable abi_reg_to_width varname =
  Option.is_some @@ abi_reg_to_width varname

let check_types_same_exn exp left right : unit =
  if 0 <> Typed.compare left right
  then
    failwith @@ sprintf "Types do not match on lhs, rhs of binop: %a" Exp.pps exp
  else ()

let is_arith_op = function
  | Bil.PLUS
    | Bil.MINUS
    | Bil.TIMES
    | Bil.DIVIDE
    | Bil.SDIVIDE
    | Bil.MOD
    | Bil.SMOD
    | Bil.AND
    | Bil.OR
    | Bil.XOR -> true
  | _ -> false

let is_lop = function
  | Bil.EQ
    | Bil.NEQ
    | Bil.LT
    | Bil.LE
    | Bil.SLT
    | Bil.SLE -> true
  | _ -> false

let is_shift_op = function
  | Bil.LSHIFT
    | Bil.RSHIFT
    | Bil.ARSHIFT -> true
  | _ -> false

let rec determine_types_from_exp (exp : Bil.exp) abi_typer : typd option ST.t =
  match exp with
  | Bil.Load (_, _, _, sz) ->
     ST.return @@ Option.return @@ Typed.Bitvector (Common.int_of_sz sz)
  | Bil.Store (_, _, store_data, _, sz) -> ST.return None
  | Bil.BinOp (op, left, right) ->
     determine_types_from_exp left abi_typer >>= fun left_typd ->
     determine_types_from_exp right abi_typer >>= fun right_typd ->
     let () = match left_typd, right_typd with
       | Some left_typd, Some right_typd when is_lop op || is_arith_op op ->
          check_types_same_exn exp left_typd right_typd
       | _ -> ()
     in
     let final_typd = match left_typd, right_typd with
       | _ when is_lop op -> Some (Typed.Bitvector 1)
       | Some l, _ when is_arith_op op -> Some l
       | _, Some r when is_arith_op op -> Some r
       | Some l, _ when is_shift_op op -> Some l
       | _, _ when is_shift_op op ->
          failwith @@ sprintf "Can't type shift expr %a" Exp.pps exp
       | Some l, _ -> Some l
       | _, Some r -> Some r
       | None, None -> None
     in
     ST.return final_typd
  | Bil.UnOp (_, left) -> determine_types_from_exp left abi_typer
  | Bil.Var var ->
     let name = Var.name var in
     if is_abi_typable abi_typer name
     then
       let width = Option.value_exn (abi_typer name) in
       ST.return @@ Option.return @@ Typed.Bitvector width
     else
       has_type_in_env name >>= fun already_typed ->
       if already_typed
       then
         get_type_in_env name >>| Option.return
       else
         ST.return None
  | Bil.Int word ->
     ST.return @@ Option.return @@ Typed.Bitvector (Word.bitwidth word)
  | Bil.Cast (_, n, _) ->
     ST.return @@ Option.return @@ Typed.Bitvector n
  | Bil.Let (_, _, _) -> ST.return None
  | Bil.Unknown (_, _) -> ST.return None
  | Bil.Ite (_, then', else') ->
     determine_types_from_exp then' abi_typer >>= fun then_typd ->
     determine_types_from_exp else' abi_typer >>= fun else_typd ->
     begin
       match then_typd, else_typd with
       | Some then_typd, Some else_typd ->
          let () = check_types_same_exn exp then_typd else_typd in
          ST.return @@ Option.return then_typd 
       | Some then_typd, _ ->
          ST.return @@ Option.return then_typd 
       | _, Some else_typd ->
          ST.return @@ Option.return else_typd 
       | _ ->
          failwith @@
            sprintf "Can't determine type of ITE expr: %a" Exp.pps exp
     end
  | Bil.Extract (hi, lo, left) ->
     let final_typd = Typed.Bitvector (hi - lo + 1) in
     ST.return @@ Some final_typd
  | Bil.Concat (left, right) ->
     determine_types_from_exp left abi_typer >>= fun left_typd ->
     determine_types_from_exp right abi_typer >>= fun right_typd ->
     match left_typd, right_typd with
     | Some left_typd, Some right_typd ->
        ST.return @@ Option.return @@ Typed.concat left_typd right_typd
     | _ ->
        failwith @@
          sprintf "Can't type a concat of two unknown types: %a" Exp.pps exp

let var_is_mem_var : var -> bool = String.equal "mem"

let make_initial () : State.t =
  { types = TypedMap.empty }

let process_def def abi_typer : unit ST.t =
  let lhs = Var.name @@ Def.lhs def in
  let rhs = Def.rhs def in
  determine_types_from_exp rhs abi_typer >>= fun rhs_type ->
  let rhs_not_typed = Option.is_none rhs_type in
  if rhs_not_typed || var_is_mem_var lhs
  then ST.return ()
  else
    let rhs_type = Option.value_exn rhs_type in
    has_type_in_env lhs >>= fun lhs_has_type -> 
    if lhs_has_type
    then
      get_type_in_env lhs >>| fun lhs_expected_type ->
      check_types_same_exn rhs lhs_expected_type rhs_type
    else
      set_type_for_var lhs rhs_type
               
let run defs abi_typer =
  let rec process_all_defs = function
    | [] -> ST.return ()
    | d :: ds ->
       process_def d abi_typer >>= fun () ->
       process_all_defs ds
  in
  let initial_type_state = make_initial () in
  let compute_all_types = process_all_defs defs in
  let (), final_type_state = ST.run compute_all_types initial_type_state in
  final_type_state.types

let get_type varname type_state = Map.find type_state varname

let get_bitwidth varname type_state =
  Option.bind
    (get_type varname type_state)
    ~f:(fun (Typed.Bitvector sz) -> Some sz)

let get_all_typed_vars = Map.keys

let print = State.print 
  
    
