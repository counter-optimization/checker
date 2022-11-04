open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

let package = "uarch-checker"
let name = "interval"
let desc = "Interval type/domain for abstract interpretation"
let width = 128 (* bits *)

module Endpoint = struct
  type t = Num of Word.t | PosInf | NegInf [@@deriving sexp, bin_io]

  let of_word x = Num x
  let pinf = PosInf
  let ninf = NegInf
  let num x = Num x

  let zero = Num (Word.zero width)
  let one = Num (Word.of_int ~width 1)
  let negone = Num (Word.of_int ~width (-1))

  let (>>=) x f =
    match x with
    | PosInf -> PosInf
    | NegInf -> NegInf
    | Num w -> Num (f w)

  let neg e =
    match e with
    | PosInf -> NegInf
    | NegInf -> PosInf
    | Num w -> Num (Word.neg w)

  let lnot = function
    | PosInf -> zero
    | NegInf -> zero
    | Num x -> Num (Word.lnot x)

  (* is this right? what is +oo + -oo ?? *)
  let add e1 e2 =
    match e1, e2 with
    | PosInf, NegInf -> zero
    | NegInf, PosInf -> zero
    | PosInf, _ -> PosInf
    | _, PosInf -> PosInf
    | NegInf, _ -> NegInf
    | _, NegInf -> NegInf
    | Num x, Num y -> Num (Word.add x y)

  let sub e1 e2 =
    match e1, e2 with
    | PosInf, PosInf -> zero
    | NegInf, NegInf -> zero
    | NegInf, _ -> NegInf
    | PosInf, _ -> PosInf
    | Num x, Num y -> Num (Word.sub x y)
    | Num _, NegInf -> PosInf
    | Num _, PosInf -> NegInf

  let mul e1 e2 =
    match e1, e2 with
    | PosInf, PosInf -> PosInf
    | NegInf, NegInf -> PosInf
    | NegInf, PosInf -> NegInf
    | PosInf, NegInf -> NegInf
    | Num x, Num y -> Num (Word.mul x y)
    | _, PosInf -> PosInf
    | PosInf, _ -> PosInf
    | _, NegInf -> NegInf
    | NegInf, _ -> NegInf

  (* Not caring if e2 is Num 0 *)
  let divide e1 e2 =
    let wzero = Word.zero width in
    match e1, e2 with
    | PosInf, PosInf -> one
    | NegInf, NegInf -> one
    | PosInf, NegInf -> negone
    | NegInf, PosInf -> negone
    | PosInf, Num x -> if Word.(<) x wzero then NegInf else PosInf
    | NegInf, Num x -> if Word.(<) x wzero then PosInf else NegInf
    | Num x, Num y -> Num (Word.div x y)
    | Num _, PosInf -> zero
    | Num _, NegInf -> zero

  let smod e1 e2 =
    match e1, e2 with
    | PosInf, PosInf -> PosInf
    | PosInf, NegInf -> NegInf
    | NegInf, NegInf -> PosInf
    | NegInf, PosInf -> NegInf
    | PosInf, Num x -> e2
    | NegInf, Num x -> neg e2
    | Num x, PosInf -> e1
    | Num x, NegInf -> neg e1
    | Num x, Num y -> Num (Word.modulo x y)

  let lshift e1 e2 =
    match e1, e2 with
    | Num x, Num y -> Num (Word.lshift x y)
    | Num x, PosInf -> zero
    | Num x, NegInf -> zero (* ?? *)
    | _ -> PosInf

  let rshift e1 e2 =
    match e1, e2 with
    | Num x, Num y -> Num (Word.rshift x y)
    | Num x, PosInf -> zero
    | Num x, NegInf -> zero (* ?? *)
    | _ -> PosInf
  
  let arshift e1 e2 =
    match e1, e2 with
    | Num x, Num y -> Num (Word.arshift x y)
    | Num x, PosInf -> zero
    | Num x, NegInf -> zero (* ?? *)
    | _ -> PosInf

  let logand e1 e2 =
    match e1, e2 with
    | Num x, Num y -> Num (Word.logand x y)
    | _, PosInf -> e1
    | PosInf, _ -> e2
    | NegInf, _ -> e2
    | _, NegInf -> e1

  let logor e1 e2 =
    match e1, e2 with
    | Num x, Num y -> Num (Word.logor x y)
    | Num _, PosInf -> PosInf
    | PosInf, PosInf -> PosInf
    | PosInf, Num _ -> PosInf
    | _ -> NegInf

  let logxor e1 e2 =
    match e1, e2 with
    | Num x, Num y -> Num (Word.logxor x y)
    | PosInf, PosInf -> zero
    | NegInf, NegInf -> zero
    | _ -> PosInf

  let smod_single (e : t) (sz : int) : t =
    let max = Int.pow 2 sz in
    let min = (-max) in
    match e with
    | PosInf -> Num (Word.of_int ~width (max - 1))
    | NegInf -> Num (Word.of_int ~width min)
    | Num w ->
       let m = Word.of_int ~width sz in
       Num (Word.modulo w m)

  (** Ordering *)
  let compare (x : t) (y : t) : int =
    match (x, y) with
    | (PosInf, PosInf) -> 0
    | (PosInf, _) -> 1
    | (_, PosInf) -> -1
    | (NegInf, NegInf) -> 0
    | (NegInf, _) -> -1
    | (_, NegInf) -> 1
    | (Num lo, Num hi) -> Word.compare lo hi

  let compare_poset (x : t) (y : t) : KB.Order.partial =
    let open KB.Order in 
    match compare x y with
    | -1 -> LT
    | 0 -> EQ
    | 1 -> GT
    | _ -> failwith "compare_poset unexpected value"

  let max x y =
    match compare x y with
    | 0 -> x
    | -1 -> y
    | 1 -> x
    | _ -> failwith "max unexpected value"

  let min x y =
    match compare x y with
    | 0 -> x
    | -1 -> x
    | 1 -> y
    | _ -> failwith "max unexpected value"

  let max4 a b c d =
    max a b |> max c |> max d

  let min4 a b c d =
    min a b |> min c |> min d

  let to_string x =
    match x with
    | PosInf -> "+oo"
    | NegInf -> "-oo"
    | Num w -> Word.to_string w
end

type t = Bot | Interval of Endpoint.t * Endpoint.t [@@deriving sexp, bin_io]

let empty = Bot
let bot = Bot
let top = Interval (NegInf, PosInf)
let w = Word.of_int ~width

(** Conversion functions *)
let of_ints (lo : int) (hi : int) : t =
  let lo' = Endpoint.of_word (w lo) in
  let hi' = Endpoint.of_word (w hi) in
  Interval (lo', hi')

let of_int lo_and_hi = of_ints lo_and_hi lo_and_hi

let of_words lo hi = Interval (Endpoint.of_word lo, Endpoint.of_word hi)

let of_word lo_and_hi = of_words lo_and_hi lo_and_hi

let to_string = function
  | Bot -> "_|_"
  | Interval (lo, hi) ->
     let lo = Endpoint.to_string lo in
     let hi = Endpoint.to_string hi in
     Format.sprintf "[%s, %s]" lo hi

let b0 = of_int 0
let b1 = of_int 1
  
(** Comparison/Domain *)
let order x y : KB.Order.partial =
  let open KB.Order in
  match (x, y) with
  | Bot, Bot -> EQ
  | Bot, _ -> LT
  | _, Bot -> GT
  | Interval (lo, hi), Interval (lo', hi') ->
     let lows = Endpoint.compare lo lo' in
     let highs = Endpoint.compare hi hi' in
     match (lows, highs) with
     | 0, 0 -> EQ (* the same interval *)
     | -1, 0 -> GT (* GT, e.g., [-4, 0] >= [-3, 0] bc ordering is inclusion *)
     | -1, 1 -> GT
     | 0, 1 -> GT
     | 0, -1 -> LT (* [0, 1] <= [0, 2] *)
     | 1, -1 -> LT
     | 1, 0 -> LT
     | -1, -1 -> NC
     | 1, 1 -> NC
     | _ -> failwith "unhandled case in comparing intervals"

let equal (x : t) (y : t) : bool =
  match order x y with
  | KB.Order.EQ -> true
  | _ -> false

let join x y =
  match x, y with
  | Bot, Bot -> Bot
  | Bot, _ -> y
  | _, Bot -> x
  | Interval (lo, hi), Interval (lo', hi') ->
     Interval (Endpoint.min lo lo', Endpoint.max hi hi')

let to_string x =
  match x with
  | Bot -> "_|_"
  | Interval (lo, hi) ->
     let lo = Endpoint.to_string lo in
     let hi = Endpoint.to_string hi in
     Format.sprintf "[%s, %s]" lo hi

(** Operators *)
let unop f = function
  | Bot -> Bot
  | Interval (lo, hi) ->
     let nlo = f lo in
     let nhi = f hi in
     let smallest = Endpoint.min nlo nhi in
     let biggest = Endpoint.max nlo nhi in
     Interval (smallest, biggest)
  
let neg = unop Endpoint.neg
let lnot = unop Endpoint.lnot

let binop f i1 i2 =
  match i1, i2 with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Interval (lo, hi), Interval (lo', hi') ->
     let x1 = f lo lo' in
     let x2 = f lo hi' in
     let x3 = f hi lo' in
     let x4 = f hi hi' in
     let min = Endpoint.min4 x1 x2 x3 x4 in
     let max = Endpoint.max4 x1 x2 x3 x4 in
     Interval (min, max)

let low n = function
  | Bot -> Bot
  | Interval (lo, hi) ->
     let x = Endpoint.smod_single lo n in
     let y = Endpoint.smod_single hi n in
     let smallest = Endpoint.min x y in
     let biggest = Endpoint.max x y in
     Interval (smallest, biggest)

(* This could be more precise *)
let high n = function
  | Bot -> Bot
  | Interval (lo, hi) -> top
     
let unsigned n x = x
let signed n x = x
               
let add = binop Endpoint.add
let sub = binop Endpoint.sub
let mul = binop Endpoint.mul
let div = binop Endpoint.divide
let sdiv = binop Endpoint.divide
let umod x y =
  match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Interval (lo, hi), Interval (lo', hi') ->
     let lo = Endpoint.zero in
     let hi = Endpoint.max lo' hi' in
     Interval (lo, hi)
let smod = binop Endpoint.smod
let lshift = binop Endpoint.lshift
let rshift = binop Endpoint.rshift
let arshift = binop Endpoint.arshift
let logand = binop Endpoint.logand
let logor = binop Endpoint.logor
let logxor = binop Endpoint.logxor

let booleq x y =
  if equal x y then b1 else b0

let boolneq x y =
  if equal x y then b0 else b1

let boollt x y =
  match order x y with
  | LT -> b1
  | _ -> b0

let boolle x y =
  match order x y with
  | LT -> b1
  | EQ -> b1
  | _ -> b0

let boolslt x y =
  match order x y with
  | LT -> b1
  | _ -> b0

let boolsle x y =
  match order x y with
  | LT -> b1
  | EQ -> b1
  | _ -> b0

let could_be_true x =
  match order b1 x with
  | LT -> true
  | EQ -> true
  | _ -> false

(** KB related declares and defines *)
let domain =
  KB.Domain.define
    ~inspect:sexp_of_t
    ~join:(fun x y -> Ok (join x y))
    ~empty
    ~order
    "interval-domain"
