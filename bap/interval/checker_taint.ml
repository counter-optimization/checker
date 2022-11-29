open Core_kernel
open Bap.Std
open Graphlib.Std
open Common

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB
module Key = Common.DomainKey

module Analysis : NumericDomain = struct
  type t = Notaint | Taint [@@deriving bin_io, sexp]

  let key : t Key.k = Key.create "taint"

  let get : type a. a Key.k -> (t -> a) option = fun k ->
    match Key.eq_type k key with
    | Eq -> Some (fun x -> x)
    | Neq -> None

  let set : type a. a Key.k -> a -> t -> a = fun k other replace ->
    match Key.eq_type k key with
    | Eq -> replace
    | Neq -> other

  let bot = Notaint
  let top = Taint
  let make_top _width _signed = top

  let b1 = top
  let b0 = bot

  let order x y =
    match x, y with
    | Taint, Taint -> KB.Order.EQ
    | Notaint, Notaint -> KB.Order.EQ
    | Notaint, Taint -> KB.Order.LT
    | Taint, Notaint -> KB.Order.GT

  let compare x y : int =
    let open KB.Order in
    match order x y with
    | LT -> -1
    | EQ -> 0
    | GT -> 1
    | NC -> -1

  let equal x y =
    match order x y with
    | KB.Order.EQ -> true
    | _ -> false

  let join x y =
    match x, y with
    | Taint, _ -> Taint
    | _, Taint -> Taint
    | _ -> Notaint

  let meet x y =
    match x, y with
    | Notaint, _ -> Notaint
    | _, Notaint -> Notaint
    | _ -> Taint

  let contains x y =
    match order x y with
    | KB.Order.EQ -> true
    | KB.Order.LT -> true
    | _ -> false

  let binop = join
  let add = binop
  let sub = binop
  let mul = binop
  let div = binop
  let sdiv = binop
  let umod = binop
  let smod = binop
  let lshift = binop
  let rshift = binop
  let arshift = binop
  let logand = binop
  let logor = binop
  let logxor = binop

  let ident = fun x -> x
  let neg = ident
  let lnot = ident

  let booleq = binop
  let boolneq = binop
  let boollt = binop
  let boolle = binop
  let boolslt = binop
  let boolsle = binop

  let could_be_true _ = true
  let could_be_false _ = true

  let drop_first x y = y
  let unsigned = drop_first
  let signed = drop_first
  let low = drop_first
  let high = drop_first

  let extract exp hi lo = exp
  let concat = join

  let to_string = function
    | Taint -> "Taint"
    | Notaint -> "Notaint"
  let of_int ?(width = 64) _ = Notaint
  let of_word _ = Notaint
  let of_z ?(width : int = 64) _ = Notaint
  let bitwidth x = -1
end
