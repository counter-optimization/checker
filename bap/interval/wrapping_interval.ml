open Core
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

let to_string (intvl : t) : string =
  match intvl with
  | Bot -> "_|_"
  | Interval {lo; hi; width; signed} ->
     Format.sprintf "[%s, %s] (width: %s, signed: %s)"
       (Z.to_string lo)
       (Z.to_string hi)
       (Int.to_string width)
       (Bool.to_string signed)

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
       Interval {lo=lo;
                 hi=hi;
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

let binop op left right : t =
  match left, right with
  | Interval {lo=lo1; hi=hi1; width=width1; signed=signed1},
    Interval {lo=lo2; hi=hi2; width=width2; signed=signed2} ->
     (* let () = if width1 <> width2 *)
     (*          then Format.printf "Width mismatch, width1: %d, width2: %d\n%!" width1 width2 *)
     (*          else () in *)
     let x1 = op lo1 lo2 in
     let x2 = op lo1 hi2 in
     let x3 = op hi1 lo2 in
     let x4 = op hi1 hi2 in
     let new_lo = min4 x1 x2 x3 x4 in
     let new_hi = max4 x1 x2 x3 x4 in
     let res = Interval {lo = new_lo;
                         hi = new_hi;
                         width = width1;
                         signed = signed1 || signed2}
     in
     let wrapping_range = range ~width:width1 ~signed:signed1 in
     wrap_intvl res wrapping_range
  | _, _ -> Bot

(* we assume that the cryptographic code is correct and safe of
   common programming errors like div by zero. because of this, if
   the divisor is an interval like [0, UINT32_MAX], then the divisor
   can be changed to [1, UINT32_MAX] since we know 0 can't have happened.
   Ditto for [INT32_MIN, 0].
   If 0 is not the endpoints of the interval, then a div by zero won't happen
   in *this* code, but we don't change the intervals since we can't put a hole
   in the interval anyways. *)
let make_rhs_zero_safe binop_fn left right : t =
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
        binop binop_fn left new_right
     | false, true ->
        let new_right = Interval { lo; hi = Z.sub hi Z.one; width; signed } in
        binop binop_fn left new_right
     | false, false -> binop binop_fn left right)
  | Bot -> failwith "in Wrapping_interval.safe_div, right is bot"

let unop op intvl =
  match intvl with
  | Interval r ->
     let x1 = op r.lo in
     let x2 = op r.hi in
     Interval { r with lo = Z.min x1 x2; hi = Z.max x1 x2 }
  | Bot -> Bot

let shift_wrapper op x y = op x (Z.to_int y)

let add = binop Z.add
let sub = binop Z.sub
let mul = binop Z.mul
let div = make_rhs_zero_safe Z.div
let sdiv = make_rhs_zero_safe Z.div
let umod = make_rhs_zero_safe Z.rem
let smod = make_rhs_zero_safe Z.rem
let lshift = binop @@ shift_wrapper Z.shift_left
let rshift = binop @@ shift_wrapper Z.shift_right
let arshift = binop @@ shift_wrapper Z.shift_right_trunc
let logand = binop Z.logand
let logor = binop Z.logor
let logxor = binop Z.logxor

let neg = unop Z.neg
let lnot = unop Z.lognot

let extract exp h l =
  match exp with 
  | Interval r ->
     let width = h - l + 1 in
     
     let x1 = Z.extract r.lo l width in
     let x2 = Z.extract r.hi l width in
     
     let new_intvl = Interval { lo = Z.min x1 x2;
                                hi = Z.max x1 x2;
                                width;
                                signed = r.signed } in
     let target_range = range ~width ~signed:r.signed in
     
     wrap_intvl new_intvl target_range
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
     (* let () = printf "in wrapping_interval.high, len is %d, width is %d\n%!" len width in *)
     let x1 = Z.extract lo offs len in
     let x2 = Z.extract hi offs len in
     Interval { lo = Z.min x1 x2;
                hi = Z.max x1 x2;
                width = len;
                signed = false }

let length : t -> int option = function
  | Interval {lo; hi; width; signed} ->
     let zdiff = Z.sub hi lo in
     Some (Z.to_int zdiff)
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
      loop ~hi:(Z.sub hi Z.one) ~lo ~res:res'
  in
  match intvl with
  | Bot -> Or_error.error_string "In WI.to_list, tried to to_list bottom elt"
  | Interval {lo; hi; width; signed} -> Ok (loop ~hi ~lo ~res:[])

let of_word (w : word) : t =
  let width = Word.bitwidth w in
  let z_val = Word.to_bitvec w |> Bitvec.to_bigint in
  of_z ~width z_val

  (* if width = 64 *)
  (* then match Word.to_int64 w with *)
  (*      | Error _ -> failwith "in Wrapping_interval.of_word, couldn't convert word to int64" *)
  (*      | Ok i -> of_int64 i *)
  (* else *)
  (*   if width < 64 *)
  (*   then match Word.to_int w with *)
  (*        | Error _ -> failwith "in Wrapping_interval.of_word, couldn't convert word to int64" *)
  (*        | Ok i -> of_int ~width i *)
  (*   else *)
  (*     failwith "can't handle integers > 64 bits in Wrapping_interval.of_word" *)

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
     let err_msg = Format.sprintf
                     "Couldn't get bitwidth for bottom value %s"
                     (to_string x) in
     failwith err_msg

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
