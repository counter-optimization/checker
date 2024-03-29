open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

let package = "uarch-checker"
let name = "interval"
let desc = "Interval type/domain for abstract interpretation"
let width = 128 (* bits *)

type range = {lo: Z.t; hi: Z.t; width: int; signed: bool} (* [@@deriving bin_io] *)
type t = Bot | Interval of range (* [@@deriving bin_io] *)

(** constants and lattice-based values *)
let empty = Bot
let bot = Bot

let make_b1 width signed = Interval {lo=Z.one; hi=Z.one; width; signed}
let make_b0 width signed = Interval {lo=Z.zero; hi=Z.zero; width; signed}
let b1_u64 = make_b1 64 false
let b0_u64 = make_b0 64 false
let b1 = make_b1 1 false
let b0 = make_b0 1 false

let make_top width signed : t =
  let two = Z.of_int 2 in
  let range_lo = if signed then Z.neg (Z.pow two (width - 1)) else Z.zero in
  let range_hi = Z.sub (Z.pow two (if signed then width - 1 else width)) Z.one in
  Interval {lo = range_lo; hi = range_hi; width = width; signed = signed}

let top : t = make_top 64 false

(** returns (min x y, max x y) with only one comparison *)
let fastminmax (x : Z.t) (y : Z.t) : Z.t * Z.t =
  if Z.lt x y then (x, y) else (y, x)

let to_string (intvl : t) : string =
  match intvl with
  | Bot -> "_|_"
  | Interval {lo; hi; width; signed} ->
    Format.sprintf "[%s, %s] (width: %s, signed: %s)"
      (Z.to_string lo)
      (Z.to_string hi)
      (Int.to_string width)
      (Bool.to_string signed)

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

let compare x y : int =
  let open KB.Order in
  match order x y with
  | LT -> -1
  | EQ -> 0
  | GT -> 1
  | NC -> -1

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

let meet x y : t =
  match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Interval {lo=lo1; hi=hi1; width=width1; signed=signed1},
    Interval {lo=lo2; hi=hi2; width=width2; signed=signed2} ->
    let lo = Z.max lo1 lo2 in
    let hi = Z.min hi1 hi2 in
    if Z.gt lo hi
    then Bot
    else
      Interval { lo;
                 hi;
                 width=Int.max width1 width2;
                 signed = false}

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

module Limits = struct
  let umin = 0

  let umax1 = Z.one
  let umax8 = Z.pred @@ Z.of_int 256
  let umax16 = Z.pred @@ Z.of_int 65536
  let umax32 = Z.pred @@ Z.of_int 4294967296
  let umax64 = Z.pred @@ Z.shift_left umax32 32
  let umax128 = Z.pred @@ Z.shift_left umax64 64
  let umax256 = Z.pred @@ Z.shift_left umax128 128
  let umax512 = Z.pred @@ Z.shift_left umax256 256

  let smin8 = Z.of_int (-128)
  let smin16 = Z.of_int (-32768)
  let smin32 = Z.of_int (-2147483648)
  let smin64 = Z.of_string "-9223372036854775808"
  let smin128 = Z.of_string "-170141183460469231731687303715884105728"
  let smin256 = Z.of_string "-57896044618658097711785492504343953926634992332820282019728792003956564819968"
  let smin512 = Z.of_string "-6703903964971298549787012499102923063739682910296196688861780721860882015036773488400937149083451713845015929093243025426876941405973284973216824503042048"

  let smax8 = Z.lognot smin8
  let smax16 = Z.lognot smin16
  let smax32 = Z.lognot smin32
  let smax64 = Z.lognot smin64
  let smax128 = Z.lognot smin128
  let smax256 = Z.lognot smin256
  let smax512 = Z.lognot smin512

  let width_to_umax = function
    | 1 -> umax1
    | 8 -> umax8
    | 16 -> umax16
    | 32 -> umax32
    | 64 -> umax64
    | 128 -> umax128
    | 256 -> umax256
    | 512 -> umax512
    | n -> Z.(pred (pow (of_int 2) n))

  let width_to_smax = function
    | 1 -> failwith "Can't width_to_smax width 1 bitvector"
    | 8 -> smax8
    | 16 -> smax16
    | 32 -> smax32
    | 64 -> smax64
    | 128 -> smax128
    | 256 -> smax256
    | 512 -> smax512
    | n ->
      let a = n - 1 in
      Z.(pred (pow (of_int 2) a))

  let width_to_smin = function
    | 1 -> failwith "Can't width_to_smax width 1 bitvector"
    | 8 -> smin8
    | 16 -> smin16
    | 32 -> smin32
    | 64 -> smin64
    | 128 -> smin128
    | 256 -> smin256
    | 512 -> smin512
    | n ->
      let a = n - 1 in
      Z.(neg (pow (of_int 2) a))

  let unsigned_truncate (num : Z.t) (width : int) : Z.t =
    let umax = width_to_umax width in
    let umax_plus_one = Z.succ umax in
    let rec loop n =
      if Z.lt n Z.zero
      then loop Z.(n + umax_plus_one)
      else if Z.gt n umax
      then Z.(logand n umax)
      else n
    in
    loop num

  let signed_to_unsigned lo hi width : Z.t * Z.t =
    let lo = unsigned_truncate lo width in
    let hi = unsigned_truncate hi width in
    fastminmax lo hi

  let signed_truncate num width : Z.t =
    let smax = width_to_smax width in
    let smin = width_to_smin width in
    let rec loop n =
      let () = printf "Num is %s\n%!" @@ Z.to_string n in
      if Z.lt n smin
      then loop (Z.(smax - ((pred smin) - n))) 
      else if Z.gt n smax
      then loop Z.((n - (succ smax)) + smin)
      else n
    in
    loop num

  let unsigned_to_signed lo hi width : Z.t * Z.t =
    let lo = signed_truncate lo width in
    let hi = signed_truncate hi width in
    fastminmax lo hi
end

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
(** binop does signed/unsigned casting like how C
    specifies for an implementation that uses the 
    natural base2 binary twos complement for 8,16,32,64,128,256,512
    bit bitvectors.
    this means no padding bits for unsigned integer types
    and no padding bits for signed integer types tho one sign
    bit as the MSB like normal two's complement *)
let binop ?(signed : bool = false) opname op left right : t =
  match left, right with
  | Interval {lo=lo1; hi=hi1; width=width1; signed=signed1},
    Interval {lo=lo2; hi=hi2; width=width2; signed=signed2} ->
    let target_width, width1, width2 =
      if width1 = width2
      then width1, width1, width2
      else
        let bigwidth = Int.max width1 width2 in
        bigwidth, bigwidth, bigwidth
    in
    if signed
    then
      let lo1, hi1 = if signed1
        then lo1, hi1
        else Limits.unsigned_to_signed lo1 hi1 target_width
      in
      let lo2, hi2 = if signed2
        then lo2, hi2
        else Limits.unsigned_to_signed lo2 hi2 target_width
      in
      let x1 = Limits.signed_truncate (op lo1 lo2) target_width in
      let x2 = Limits.signed_truncate (op lo1 hi2) target_width in
      let x3 = Limits.signed_truncate (op hi1 lo2) target_width in
      let x4 = Limits.signed_truncate (op hi1 hi2) target_width in
      let new_lo = min4 x1 x2 x3 x4 in
      let new_hi = max4 x1 x2 x3 x4 in
      Interval {
        lo = new_lo;
        hi = new_hi;
        width = target_width;
        signed = true
      }
    else (* unsigned *)
      let lo1, hi1 = if signed1
        then Limits.signed_to_unsigned lo1 hi1 target_width
        else lo1, hi1
      in
      let lo2, hi2 = if signed2
        then Limits.signed_to_unsigned lo2 hi2 target_width
        else lo2, hi2
      in
      let x1 = Limits.unsigned_truncate (op lo1 lo2) target_width in
      let x2 = Limits.unsigned_truncate (op lo1 hi2) target_width in
      let x3 = Limits.unsigned_truncate (op hi1 lo2) target_width in
      let x4 = Limits.unsigned_truncate (op hi1 hi2) target_width in
      let new_lo = min4 x1 x2 x3 x4 in
      let new_hi = max4 x1 x2 x3 x4 in
      Interval {
        lo = new_lo;
        hi = new_hi;
        width = target_width;
        signed = false
      }
  | _, _ -> Bot

(* we assume that the cryptographic code is correct and safe of
   common programming errors like div by zero. because of this, if
   the divisor is an interval like [0, UINT32_MAX], then the divisor
   can be changed to [1, UINT32_MAX] since we know 0 can't have happened.
   Ditto for [INT32_MIN, 0].
   If 0 is not the endpoints of the interval, then a div by zero won't happen
   in *this* code, but we don't change the intervals since we can't put a hole
   in the interval anyways. *)
let make_rhs_zero_safe ?(signed : bool = false) opname binop_fn left right : t =
  match right with
  | Interval {lo; hi; width; signed} ->
    let zero = Z.zero in
    let lo_is_zero = Z.equal lo zero in
    let hi_is_zero = Z.equal hi zero in
    (match lo_is_zero, hi_is_zero with
     | true, true ->
       failwith "in Wrapping_interval.safe_div, div by constant zero"
     | true, false ->
       let new_right = Interval { lo = Z.add lo Z.one; hi; width; signed } in
       binop opname binop_fn left new_right
     | false, true ->
       let new_right = Interval { lo; hi = Z.sub hi Z.one; width; signed } in
       binop ~signed opname binop_fn left new_right
     | false, false -> binop ~signed opname binop_fn left right)
  | Bot -> failwith "in Wrapping_interval.safe_div, right is bot"

let unop op intvl =
  match intvl with
  | Interval r ->
    let x1 = Limits.unsigned_truncate (op r.lo) r.width in
    let x2 = Limits.unsigned_truncate (op r.hi) r.width in
    let lo, hi = fastminmax x1 x2 in
    Interval { lo; hi; width = r.width; signed = false }
  | Bot -> Bot

(* TODO: is this x86 specific? *)
let shift_wrapper op x y =
  let masked = Z.logand y @@ Z.of_int 63 in
  let masked_int = Z.to_int masked in
  op x masked_int

let add = binop "add" Z.add
let sub = binop "sub" Z.sub
let mul = binop "mul" Z.mul
let div = make_rhs_zero_safe "div" Z.div
let sdiv = make_rhs_zero_safe "sdiv" ~signed:true Z.div
let umod = make_rhs_zero_safe "umod" Z.rem
let smod = make_rhs_zero_safe "smod" ~signed:true Z.rem
let lshift = binop "lshift" @@ shift_wrapper Z.shift_left
let rshift = binop "rshift" @@ shift_wrapper Z.shift_right
let arshift = binop "arshift" @@ shift_wrapper Z.shift_right_trunc
let logand = binop "logand" Z.logand
let logor = binop "logor" Z.logor
let logxor = binop "logxor" Z.logxor

let neg = unop Z.neg

let lnot = function
  | Interval {lo; hi; width; signed} ->
    if width <= 64 && Z.fits_int64 lo && Z.fits_int64 hi
    then
      let lo64 = Z.to_int64 lo in
      let hi64 = Z.to_int64 hi in
      let lo_w = Word.of_int64 ~width lo64 in
      let hi_w = Word.of_int64 ~width hi64 in
      let lo_lnot = Word.lnot lo_w in
      let hi_lnot = Word.lnot hi_w in
      let max = Word.max lo_lnot hi_lnot in
      let min = Word.min lo_lnot hi_lnot in
      let max_int64 = match Word.to_int64 max with
        | Ok x -> x
        | Error e -> failwith @@ Error.to_string_hum e
      in
      let min_int64 = match Word.to_int64 min with
        | Ok x -> x
        | Error e -> failwith @@ Error.to_string_hum e
      in
      let lo = Z.of_int64 min_int64 in
      let hi = Z.of_int64 max_int64 in
      Interval {lo; hi; width; signed}
    else
      unop Z.lognot (Interval {lo; hi; width; signed})
  | Bot -> Bot

let extract exp h l =
  match exp with 
  | Interval r ->
    let width = h - l + 1 in
    let x1 = Z.extract r.lo l width in
    let x2 = Z.extract r.hi l width in
    let x1 = Limits.unsigned_truncate x1 width in
    let x2 = Limits.unsigned_truncate x2 width in
    let lo, hi = fastminmax x1 x2 in
    Interval { lo; hi; width; signed = r.signed }
  | Bot -> Bot

let concat x y =
  match x, y with
  | Interval l, Interval r ->
    let final_width = l.width + r.width in
    let final_signed = l.signed in
    let x' = Interval { l with width = final_width } in
    let y' = Interval { r with width = final_width } in
    let shift_by = Interval { lo = Z.of_int r.width;
                              hi = Z.of_int r.width;
                              width = final_width;
                              signed = final_signed } in
    let shifted = lshift x' shift_by in
    logor shifted y'
  | _, _ -> Bot


(** equality and ordering comparisons *)
(* is there any overlap at all? *)
let booleq x y : t =
  match x, y with
  | Interval {lo=lo1; hi=hi1; width=width1; signed=signed1},
    Interval {lo=lo2; hi=hi2; width=width2; signed=signed2} ->
    if Z.(leq (max lo1 lo2) (min hi1 hi2))
    then b1
    else b0
  | _ -> b0
(* failwith "in wrapping_interval.bool_eq, can't compare two non intervals" *)

let boolneq x y =
  match x, y with
  | Interval {lo=lo1; hi=hi1; width=width1; signed=signed1},
    Interval {lo=lo2; hi=hi2; width=width2; signed=signed2} ->
    if Z.(leq (max lo1 lo2) (min hi1 hi2))
    then b0
    else b1
  | _ -> b0

let boollt x y =
  match x, y with
  | Interval {lo=lo1; hi=hi1; width=width1; signed=signed1},
    Interval {lo=lo2; hi=hi2; width=width2; signed=signed2} ->
    if Z.lt lo1 hi2
    then b1
    else b0
  | _ -> b0

let boolle x y =
  match x, y with
  | Interval {lo=lo1; hi=hi1; width=width1; signed=signed1},
    Interval {lo=lo2; hi=hi2; width=width2; signed=signed2} ->
    if Z.leq lo1 hi2
    then b1
    else b0
  | _ -> b0

let boolslt = boollt 
let boolsle = boolle

let could_be_true x =
  match x with
  | Bot -> false
  | Interval l ->
    let one = make_b1 l.width l.signed in
    interval_subset one x

let could_be_false x =
  match x with
  | Bot -> false
  | Interval l ->
    let zero = make_b0 l.width l.signed in
    interval_subset zero x

(** casts *)
let unsigned len x =
  match x with
  | Bot -> Bot
  | Interval {lo; hi; width; signed} ->
    let x1 = Z.extract lo 0 (len - 1) in
    let x2 = Z.extract hi 0 (len - 1) in
    Interval { lo = Z.min x1 x2;
               hi = Z.max x1 x2;
               width = len;
               signed = false }

let signed len x =
  match x with
  | Bot -> Bot
  | Interval { lo; hi; width; signed } ->
    let x1 = Z.signed_extract lo 0 (len - 1) in
    let x2 = Z.signed_extract hi 0 (len - 1) in
    Interval { lo = Z.min x1 x2;
               hi = Z.max x1 x2;
               width = len; 
               signed = true }

let low len x =
  match x with
  | Bot -> Bot
  | Interval {lo; hi; width; signed} ->
    let x1 = Z.extract lo 0 len in
    let x2 = Z.extract hi 0 len in
    Interval { lo = Z.min x1 x2;
               hi = Z.max x1 x2;
               width = len;
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
               width = len;
               signed = false }

let length : t -> int option = function
  | Interval {lo; hi; width; signed} ->
    let zdiff = Z.sub hi lo in
    if Z.fits_int zdiff
    then Some (Z.to_int zdiff)
    else None
  | Bot -> None

let to_int (intvl : t) : int Or_error.t =
  match intvl with
  | Interval {lo; hi; width; signed} ->
    (match length intvl with
     | Some l when l = 0 && Z.fits_int lo -> Ok (Z.to_int lo)

     | Some l when l = 0 ->
       Or_error.error_string @@
       sprintf "in WI.to_int, intvl %s does not fit in ocaml int"
         (to_string intvl)

     | _ ->
       Or_error.error_string @@
       sprintf "in WI.to_int, intvl %s is not a constant"
         (to_string intvl))
  | Bot ->
    Or_error.error_string @@
    sprintf "in WI.to_int, intvl %s is domain bottom value"
      (to_string intvl) 

let to_z (intvl : t) : Z.t option =
  match intvl with
  | Interval {lo; hi; width; signed} ->
    (match length intvl with
     | Some l when l = 0 -> Some lo
     | _ -> None)
  | Bot -> None

let to_z_lo (intvl : t) : Z.t option =
  match intvl with
  | Interval {lo; hi; width; signed} -> Some lo
  | Bot -> None

let to_z_hi (intvl : t) : Z.t option =
  match intvl with
  | Interval {lo; hi; width; signed} -> Some hi
  | Bot -> None

let of_int ?(width = 64) (i : int) : t =
  Interval { lo = Z.of_int i;
             hi = Z.of_int i;
             width;
             signed = i < 0 }

let of_int64 ?(width = 64) (i : int64) : t =
  Interval { lo = Z.of_int64 i;
             hi = Z.of_int64 i;
             width;
             signed = Int64.(i < zero) }

let of_z ?(width = 64) (z : Z.t) : t =
  Interval { lo = z;
             hi = z;
             width;
             signed = Z.sign z = -1 }

let to_list (intvl : t) : t list Or_error.t =
  let rec loop ~(hi : Z.t) ~(lo : Z.t) ~(res : t list) : t list =
    if Z.equal hi lo
    then List.cons (of_z hi) res
    else
      let res' = List.cons (of_z hi) res in
      loop ~hi:(Z.sub hi Z.one) ~lo ~res:res' in
  match intvl with
  | Bot -> Or_error.error_string "[WI] tried to to_list bottom elt"
  | Interval {lo; hi; width; signed} -> Ok (loop ~hi ~lo ~res:[])

let of_word (w : word) : t =
  let width = Word.bitwidth w in
  let z_val = Word.to_bitvec w |> Bitvec.to_bigint in
  of_z ~width z_val

let of_int_tuple ?(width = 64) (x, y) =
  Interval { lo = Z.of_int x;
             hi = Z.of_int y;
             width;
             signed = x < 0 || y < 0 }

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
    "wrapping-interval-domain"

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

let get_width = function
  | Interval { lo; hi; width; signed } -> Some width
  | _ -> None

let get_sign = function
  | Interval { lo; hi; width; signed } -> Some signed
  | _ -> None

let bitwidth x =
  match get_width x with
  | Some w -> w
  | None ->
    failwith "Couldn't get bitwidth for bottom value in wrapping intervals"

let size intvl : Z.t option =
  match intvl with
  | Bot -> None
  | Interval {lo; hi; width; signed} -> Some (Z.sub hi lo)

let is_const intvl : bool =
  match size intvl with
  | None -> false
  | Some sz -> Z.equal Z.zero sz

(** Setting up domain keys for usage in InteractableNumDom *)
module Key = Domain_key.DomainKey

(* key is a module:
   struct
   type t = t
   type _ key += Key : t key
   let name = "wrapping-interval"
   end *)
let key : t Key.k = Key.create "wrapping-interval"

let get : type a. a Key.k -> (t -> a) option = fun k ->
  match Key.eq_type k key with
  | Eq -> Some (fun x -> x)
  | Neq -> None

let set : type a. a Key.k -> t -> a -> t = fun k other replace ->
  match Key.eq_type k key with
  | Eq -> replace
  | Neq -> other

(* guard operator style functions
   safe removals:
   from_ :         [                            ]
   remove:       [   ]
   remove.lo <= from_.lo && remove.hi >= from_.lo && remove.hi < from_.hi
   ---->
   from_.lo = remove.hi + 1
   from_.hi = from_.hi

   what if:
   from_:               [         ]
   remove:              [         ]
   we want all states where they could never be equal
   ---->
   Bot

   from_ : [                  ]
   remove:             [        ]
   remove.hi >= from_.hi && remove.lo <= from_.hi && remove.lo > from_.lo
   ---->
   from_.hi = remove.lo - 1
   from_.lo = from_.lo

   from_:    [                   ]
   remove:        [   ]
   ---->
   from_ = from_
   can't put holes in interval from_

   from_:       [ ]
   remove:   [       ]
   ---->
   Bot

   from_ : [       ]
   remove:                  [      ]
   ---->
   from_ = from_
*)
let try_remove_interval ~(remove : t) ~(from_ : t) : t =
  match remove, from_ with
  | _, Bot -> Bot
  | Bot, _ -> from_
  | Interval remove, Interval from_ ->
    if Z.leq remove.lo from_.lo && Z.geq remove.hi from_.hi
    then Bot
    else
    if Z.leq remove.lo from_.lo && Z.geq remove.hi from_.lo &&
       Z.lt remove.hi from_.hi
    then
      Interval { from_ with lo = Z.add remove.hi Z.one }
    else
    if Z.geq remove.hi from_.hi && Z.leq remove.lo from_.hi &&
       Z.gt remove.lo from_.lo
    then
      Interval { from_ with hi = Z.sub remove.lo Z.one }
    else
      (* both cases where there is no overlap
         OR
         from_.lo < remove.lo && remove.hi < from_.hi *)
      Interval from_


