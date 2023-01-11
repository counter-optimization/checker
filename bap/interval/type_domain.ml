open Core
open Bap.Std
open Common

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

(** Setting up domain keys for usage in InteractableNumDom *)
module Key = Common.DomainKey

type t = CellType.t
let p = CellType.Ptr
let s = CellType.Scalar
let u = CellType.Unknown
let n = CellType.Undef

let key : t Key.k = Key.create "type_domain"

let get : type a. a Key.k -> (t -> a) option = fun k ->
  match Key.eq_type k key with
  | Eq -> Some (fun x -> x)
  | Neq -> None

let set : type a. a Key.k -> t -> a -> t = fun k other replace ->
  match Key.eq_type k key with
  | Eq -> replace
  | Neq -> other

let bot = CellType.Undef

let top = CellType.Unknown

let make_top _sz _signed = top

let b1 = CellType.Unknown

let b0 = CellType.Unknown

let order = CellType.order

let compare x y =
  let open KB.Order in
  match order x y with
  | EQ -> 0
  | LT -> -1
  | GT -> 1
  | NC -> -1

let equal x y = 0 = compare x y

let join x y = CellType.join x y |> Result.ok_exn

let meet x y =
  match x, y with
  | CellType.Ptr, CellType.Ptr -> p
  | CellType.Scalar, CellType.Scalar -> s
  | _, CellType.Unknown -> x
  | CellType.Unknown, _ -> y
  | _, _ -> failwith "no meet in type domain for that combo"

let contains x y =
  let c = compare x y in
  0 = c || -1 = c

let add x y =
  match x, y with
  | CellType.Ptr, CellType.Scalar -> p
  | CellType.Scalar, CellType.Ptr -> p
  | CellType.Ptr, CellType.Ptr -> u
  | CellType.Scalar, CellType.Scalar -> s
  | CellType.Unknown, _ -> u
  | _, CellType.Unknown -> u
  | CellType.Undef, _ -> y
  | _, CellType.Undef -> x

let sub x y =
  match x, y with
  | CellType.Ptr, CellType.Scalar -> p
  | CellType.Ptr, CellType.Ptr -> s
  | CellType.Scalar, CellType.Ptr -> u
  | CellType.Scalar, CellType.Scalar -> s
  | CellType.Unknown, _ -> u
  | _, CellType.Unknown -> u
  | CellType.Undef, _ -> y
  | _, CellType.Undef -> x

let general_binop x y = s

let mul = general_binop

let div = general_binop

let sdiv = general_binop

let umod = general_binop

let smod = general_binop

let lshift = general_binop

let rshift = general_binop

let arshift = general_binop

let logand = general_binop

let logor = general_binop

let logxor = general_binop

let general_unop x = s

let neg = general_unop

let lnot = general_unop

let extract x hi lo = s

let concat = general_binop

let booleq = general_binop

let boolneq = general_binop

let boollt = general_binop

let boolle = general_binop

let boolslt = general_binop

let boolsle = general_binop

let could_be_true _ = true

let could_be_false _ = true

let general_cast n x = s

let unsigned = general_cast

let signed = general_cast

let low = general_cast

let high = general_cast

let to_string = function
  | CellType.Ptr -> "ptr"
  | CellType.Scalar -> "scalar"
  | CellType.Unknown -> "unknowntype"
  | CellType.Undef -> "undeftype"

let of_int ?width v = s

let of_word w = s

let of_z ?width z = s

let bitwidth typ = failwith "can't bitwidth on type"

let sexp_of_t = CellType.sexp_of_t
