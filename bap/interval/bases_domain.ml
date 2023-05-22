open Core
open Bap.Std
open Common

module KB = Bap_core_theory.KB

module MBase = Region
module Bases = Region.Set

type t = Region.Set.t

module Key = Domain_key.DomainKey

let key : t Key.k = Key.create "bases_domain"

let get : type a. a Key.k -> (t -> a) option = fun k ->
  match Key.eq_type k key with
  | Eq -> Some (fun x -> x)
  | Neq -> None

let set : type a. a Key.k -> t -> a -> t = fun k other replace ->
  match Key.eq_type k key with
  | Eq -> replace
  | Neq -> other

let stack = Bases.singleton MBase.Stack

let global = Bases.singleton MBase.Global

let heap = Bases.singleton MBase.Heap

let bot = Bases.empty

let top = Bases.of_list [MBase.Global; MBase.Heap; MBase.Stack]

let make_top _sz _signed = top

let not_callable_exception fn_name =
  failwith @@ sprintf "%s not callable for bases domain" fn_name

let b1 = Bases.empty

let b0 = Bases.empty

let order x y =
  let open KB.Order in
  let x_leq_y = Bases.is_subset x ~of_:y in
  let y_leq_x = Bases.is_subset y ~of_:x in
  match x_leq_y, y_leq_x with
  | true, true -> EQ
  | true, false -> LT
  | false, true -> GT
  | false, false -> NC

let compare = Bases.compare

let equal = Bases.equal

let join = Bases.union

let meet = Bases.inter

let contains x y =
  let open KB.Order in
  match order x y with
  | LT
    | EQ -> true
  | _ -> false

let add = join

let sub = join

let mul = join

let div = join

let sdiv = join

let umod = join

let smod = join

let lshift = join

let rshift = join

let arshift = join

let logand = join

let logor = join

let logxor = join

let neg = fun x -> x

let lnot = fun x -> x

let extract v hi lo = v

let concat = join

let booleq x y = Bases.empty

let boolneq x y = Bases.empty

let boollt x y = Bases.empty

let boolle x y = Bases.empty

let boolslt x y = Bases.empty

let boolsle x y = Bases.empty

let could_be_true _ = true

let could_be_false _ = true

let unsigned n v = Bases.empty

let signed n v = Bases.empty

let low n v = Bases.empty

let high n v = Bases.empty

let to_string (bases : Bases.t) : string =
  let rec loop (remainder : MBase.t list) : string =
    match List.hd remainder with
    | Some base ->
       let remainder = List.tl_exn remainder in
       if List.is_empty remainder
       then
         sprintf "%s" (MBase.to_string base)
       else
         sprintf "%s, %s" (MBase.to_string base) (loop remainder)
    | None -> ""
  in
  sprintf "(%s)" @@ loop @@ Bases.to_list bases

let of_int ?width v = Bases.empty

let of_word w = Bases.empty

let of_z ?width z = Bases.empty

let bitwidth v = not_callable_exception "bitwidth"

let sexp_of_t = Bases.sexp_of_t
