open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

let package = "uarch-checker"
let name = "interval"
let desc = "Interval type/domain for abstract interpretation"
let width = 128 (* bits *)

type range = {lo: Z.t; hi: Z.t; width: int; signed: bool} (* [@@deriving sexp, bin_io] *)
type t = Bot | Interval of range (* [@@deriving sexp, bin_io] *)

(** constants and lattice-based values *)
let empty = Bot
let bot = Bot

let make_b1 width signed = Interval {lo=Z.one; hi=Z.one; width; signed}
let make_b0 width signed = Interval {lo=Z.zero; hi=Z.zero; width; signed}
let b1_u64 = make_b1 64 false
let b0_u64 = make_b0 64 false
let b1 = make_b1 8 false
let b0 = make_b0 8 false

(* defaults to uint 64 range *)
let make_top width signed : t =
  (* -2**(N-1) or 0 *)
  let two = Z.of_int 2 in
  let range_lo = if signed then Z.neg (Z.pow two (width - 1)) else Z.zero in
  let range_hi = Z.sub (Z.pow two (if signed then width - 1 else width)) Z.one in
 Interval {lo = range_lo;
            hi = range_hi;
            width = width;
            signed = signed}

let top : t =
  make_top 64 false

(** Wraparound, integer semantics, etc *)
let wrap (n : Z.t) (r : range) : Z.t =
  let {lo; hi; width; signed} = r in
  if Z.lt n lo
  then
    let dist_from_lo = Z.abs (Z.sub n lo) in
    let unadjusted_wrap_around = Z.sub hi dist_from_lo in
    Z.add unadjusted_wrap_around Z.one
  else
    if Z.gt n hi
    then
      let dist_from_hi = Z.abs (Z.sub n hi) in
      let unadjusted_wrap_around = Z.add lo dist_from_hi in
      Z.sub unadjusted_wrap_around Z.one
    else
      n

let wrap_intvl (intvl : t) (r : range) : t =
  match intvl with
  | Bot -> Bot
  | Interval {lo; hi; width; signed} ->
     let {lo=lo'; hi=hi'; width=width'; signed=signed'} = r in
     let lo_inc = Z.succ lo in
     if Z.leq lo_inc hi' && Z.leq hi' hi
     then Interval r
     else
       if Z.leq lo_inc lo' && Z.leq lo' hi
       then Interval r
       else
         let wrapped_lo = wrap lo r in
         let wrapped_hi = wrap hi r in
         Interval { lo = Z.min wrapped_lo wrapped_hi;
                    hi = Z.max wrapped_lo wrapped_hi;
                    width = width';
                    signed = signed' }

(** Ordering *)
let order x y : KB.Order.partial =
  let open KB.Order in
  match x, y with
  | Bot, Bot -> EQ
  | Bot, _ -> LT
  | _, Bot -> GT
  | Interval {lo=lo1; hi=hi1; width=width1; signed=signed1},
    Interval {lo=lo2; hi=hi2; width=width2; signed=signed2}
    -> let lows = Z.compare lo1 lo2 in
       let highs = Z.compare hi1 hi2 in
       match lows, highs with
       | 0, 0 -> EQ
       | 0, 1 -> GT
       | 0, -1 -> LT
       | -1, 0 -> GT
       | 1, 0 -> LT
       | 1, 1 -> NC
       | -1, -1 -> NC
       | -1, 1 -> GT
       | 1, -1 -> LT
       | _ -> failwith "unhandled case in Wrapping_interval.order"

let equal x y : bool =
  match order x y with
  | EQ -> true
  | _ -> false

(* potential width/sign mismatch spot *)
let join x y : t =
  match x, y with
  | Bot, Bot -> Bot
  | Bot, _ -> y
  | _, Bot -> x
  | Interval {lo=lo1; hi=hi1; width=width1; signed=signed1},
    Interval {lo=lo2; hi=hi2; width=width2; signed=signed2} ->
     Interval { lo = Z.min lo1 lo2;
                hi = Z.max hi1 hi2;
                width = Int.max width1 width2;
                signed = false }

(* Does i1 fit completely in i2? *)
(* this is the <= subset relation on two intervals and
   only looks at interval endpoints (the values) rather
   than the type of the interval (width and signedness) *)
let range_subset r1 r2 : bool =
  match r1, r2 with
  | {lo=lo1; hi=hi1; width=width1; signed=signed1},
    {lo=lo2; hi=hi2; width=width2; signed=signed2} ->
     Z.leq lo2 lo1 && Z.leq hi1 hi2

let interval_subset i1 i2 : bool =
  match i1, i2 with
  | Interval r1, Interval r2 -> range_subset r1 r2
  | _ -> false

let range ~width ~signed : range =
  let width_pred = width - 1 in
  let lo = if signed then Z.neg (Z.pow (Z.of_int 2) width_pred) else Z.zero in
  let hi = Z.pred (Z.pow (Z.of_int 2) (if signed then width_pred else width)) in
  { lo; hi; width; signed }

let int8_range = range ~width:8 ~signed:true
let uint8_range = range ~width:8 ~signed:false
let int16_range = range ~width:16 ~signed:true
let uint16_range = range ~width:16 ~signed:false
let int32_range = range ~width:32 ~signed:true
let uint32_range = range ~width:32 ~signed:false
let int64_range = range ~width:64 ~signed:true
let uint64_range = range ~width:64 ~signed:false

let range_of_interval = function
  | Bot -> failwith "Can't get range of _|_ in range_of_interval"
  | Interval { lo; hi; width; signed } ->
     range ~width ~signed

let rank_range (r : range) : int =
  let {lo; hi; width; signed} = r in
  match width with
  | 8 -> 1
  | 16 -> 2
  | 32 -> 3
  | 64 -> 4
  | 128 -> 5
  | 256 -> 6
  | 512 -> 7
  | _ -> failwith "unknown range width in Wraping_interval.rank range"

let range_for_promote intvl : range =
  match intvl with
  | Bot -> failwith "Can't promote _|_ in range_for_promote"
  | Interval { lo; hi; width; signed } ->
     let int_rank = rank_range int32_range in
     let current_range = range_of_interval intvl in
     let current_rank = rank_range current_range in
     if current_rank < int_rank && range_subset current_range int32_range
     then int32_range
     else
       if current_rank < int_rank && range_subset current_range uint32_range
       then uint32_range
       else current_range

(* Is the final else correct? the paper makes it seem
   like the final ite if/else conditions are exclusive, but
   i haven't looked at it hard enough *)
let rec range_lub r r' : range =
  let rank = rank_range r in
  let rank' = rank_range r' in
  if rank < rank'
  then range_lub r' r
  else
    if Bool.equal r.signed r'.signed ||
         not r.signed && r'.signed ||
           r.signed && not r'.signed && range_subset r' r
    then r
    else range ~width:(r.width) ~signed:false

let min4 x1 x2 x3 x4 : Z.t =
  Z.min x1 x2
  |> Z.min x3
  |> Z.min x4

let max4 x1 x2 x3 x4 : Z.t =
  Z.max x1 x2
  |> Z.max x3
  |> Z.max x4

(** typing *)
let type_nonshift_binop op left right : range =
  let left_promote = range_for_promote left in
  let right_promote = range_for_promote right in
  range_lub left_promote right_promote

let type_shift_binop op left right : range = range_for_promote left

(** Operators *)

(* potential width/sign mismatch spot *)
let binop op left right : t =
  match left, right with
  | Interval {lo=lo1; hi=hi1; width=width1; signed=signed1},
    Interval {lo=lo2; hi=hi2; width=width2; signed=signed2} ->
     let x1 = op lo1 lo2 in
     let x2 = op lo1 hi2 in
     let x3 = op hi1 lo2 in
     let x4 = op hi1 hi2 in
     let new_lo = min4 x1 x2 x3 x4 in
     let new_hi = max4 x1 x2 x3 x4 in
     Interval {lo=new_lo; hi=new_hi; width=64; signed=false}
  | _, _ -> Bot

let unop op intvl =
  match intvl with
  | Interval {lo; hi; width; signed} ->
     let x1 = op lo in
     let x2 = op hi in
     Interval {lo=Z.min x1 x2;
               hi=Z.max x1 x2;
               width; signed}
  | Bot -> Bot

let shift_wrapper op x y = op x (Z.to_int y)

let add = binop Z.add
let sub = binop Z.sub
let mul = binop Z.mul
let div = binop Z.div
let sdiv = binop Z.div
let umod = binop Z.rem
let smod = binop Z.rem
let lshift = binop (shift_wrapper Z.shift_left)
let rshift = binop (shift_wrapper Z.shift_right)
let arshift = binop (shift_wrapper Z.shift_right_trunc)
let logand = binop Z.logand
let logor = binop Z.logor
let logxor = binop Z.logxor

let neg = unop Z.neg
let lnot = unop Z.lognot

(** equality and ordering comparisons *)
let booleq x y : t =
  match order x y with
  | KB.Order.EQ -> b1_u64
  | _ -> b0_u64

let boolneq x y =
  match order x y with
  | KB.Order.EQ -> b0_u64
  | _ -> b1_u64

let boollt x y =
  match order x y with
  | KB.Order.LT -> b1_u64
  | _ -> b0_u64

let boolle x y = logor (boollt x y) (booleq x y)
let boolslt = boollt 
let boolsle = boolle
let could_be_true x =
  match x with
  | Bot -> false
  | Interval {lo; hi; width; signed} ->
     let one = make_b1 width signed in
     interval_subset one x
let could_be_false x =
  match x with
  | Bot -> false
  | Interval {lo; hi; width; signed} ->
     let zero = make_b0 width signed in
     interval_subset zero x

(** casts *)
let unsigned len x =
  match x with
  | Bot -> Bot
  | Interval {signed=false; _} -> x
  | Interval {lo; hi; width; signed=true} ->
     let new_range = range ~width ~signed:false in
     wrap_intvl x new_range

let signed len x =
  match x with
  | Bot -> Bot
  | Interval {signed=true; _} -> x
  | Interval {lo; hi; width; signed=false} ->
     let new_range = range ~width ~signed:true in
     wrap_intvl x new_range

(* TODO: i don't think this is sound *)
let low len x =
  match x with
  | Bot -> Bot
  | Interval {lo; hi; width; signed} ->
     let x1 = Z.extract lo 0 len in
     let x2 = Z.extract hi 0 len in
     Interval { lo = Z.min x1 x2;
                hi = Z.max x1 x2;
                width = width;
                signed = false }

let high len x =
  match x with
  | Bot -> Bot
  | Interval {lo; hi; width; signed} ->
     let offs = width - len in
     let x1 = Z.extract lo offs len in
     let x2 = Z.extract hi offs len in
     Interval { lo = Z.min x1 x2;
                hi = Z.max x1 x2;
                width = width;
                signed = false }

(** conversions *)
let to_string (intvl : t) : string =
  match intvl with
  | Bot -> "_|_"
  | Interval {lo; hi; width; signed} ->
     Format.sprintf "[%s, %s] (signed: %s, width: %s)"
       (Z.to_string lo)
       (Z.to_string hi)
       (Int.to_string width)
       (Bool.to_string signed)

let of_int (i : int) : t =
  Interval { lo = Z.of_int i;
             hi = Z.of_int i;
             width = 64;
             signed = false }

let of_z (z : Z.t) : t =
  Interval { lo = z;
             hi = z;
             width = 64;
             signed = false }

let of_word (w : word) : t =
  Word.to_int_exn w |> of_int

(* serialization *)
let sexp_of_t : t -> Sexp.t = function
  | Bot -> Sexp.Atom "_|_"
  | Interval { lo; hi; width; signed } ->
     let lo_str = Z.to_string lo in
     let hi_str = Z.to_string hi in
     let l x = Sexp.List x in
     l [ l [ Sexp.Atom lo_str; Sexp.Atom hi_str ];
         Int.sexp_of_t width;
         Bool.sexp_of_t signed ]

(** BAP KB specific *)
let domain =
  KB.Domain.define
    ~inspect:sexp_of_t
    ~join:(fun x y -> Ok (join x y))
    ~empty
    ~order
    "wrapping-interval-dommain"

(** Checker specific *)
let contains = interval_subset
let contains_pow_of_two intvl =
  match intvl with
  | Interval {lo; hi; width; signed} ->
     let two = Z.of_int 2 in
     let pows_to_check = 0 :: (List.init (width - 1) ~f:succ) in
     let idx_to_pow_intvl i = of_z @@ Z.pow two i in
     let pows = List.map pows_to_check ~f:idx_to_pow_intvl in
     List.fold_left pows ~init:false ~f:(fun acc cur_pow_two ->
         acc || (contains cur_pow_two intvl))
  | _ -> false