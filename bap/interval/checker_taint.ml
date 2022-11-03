open Core
open Bap.Std
open Graphlib.Std
open Common

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

module Analysis : NumericDomain = struct
  type t = Notaint | Taint [@@deriving bin_io, sexp]

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

  let equal x y =
    match order x y with
    | KB.Order.EQ -> true
    | _ -> false

  let join x y =
    match x, y with
    | Taint, _ -> Taint
    | _, Taint -> Taint
    | _ -> Notaint

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
