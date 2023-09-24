open Core_kernel
open Bap.Std

module KB = Bap_knowledge.Knowledge
       
let package = "uarch-checker"

type t = { bw: int;
           conc: Bitvec.t;
           abs: Bitvec.t }

(** convenience. not exported out of this module *)
let (mod) = Bitvec.(mod)
let mm = Bitvec.modulus
let o = Bitvec.one
let os = Bitvec.ones
let z = Bitvec.zero
let bveq = Bitvec.equal

let os_for_bw bw = os mod (mm bw)
let is_os x bw =
  let all_ones = os_for_bw bw in
  Bitvec.equal x all_ones

let is_z x = Bitvec.equal x z

(** abstract bitvector start *)
(** Constants *)
let make_top bw _signed = { bw; conc = z; abs = os mod (mm bw) }

let make_bot bw _signed = { bw; conc = os mod (mm bw); abs = os mod (mm bw) }
                          
let top = make_top 512 false

let bot = make_bot 512 false

let one bw = { bw; conc = o; abs = z }

let b1 = one 1
           
let b0 = { bw = 1; conc = z; abs = z }

let is_top {bw;conc;abs} = is_z conc && is_os abs bw
                                          
let is_bot {bw;conc;abs} =
  let bwand = Bitvec.logand conc abs mod (mm bw) in
  not (Bitvec.equal bwand z)

(* mostly for testing convenience *)
let build bw conc maybes =
  let open Bitvec in
  { bw ; conc = !$conc; abs = !$maybes }
         
(** order *)
let equal x y = bveq x.conc y.conc && bveq x.abs y.abs

let bw_equal x y = x.bw = y.bw

let order x y : KB.Order.partial =
  let max_bw = mm (Int.max x.bw y.bw) in
  let right = Bitvec.((y.conc lor y.abs) mod max_bw) in
  let left = Bitvec.((x.conc lor x.abs) mod max_bw) in
  let x_subseq_y = Bitvec.(equal ((left lor right) mod max_bw) right) in
  let y_subseq_x = Bitvec.(equal ((right lor left) mod max_bw) left) in
  if is_bot x
  then KB.Order.LT
  else match x_subseq_y, y_subseq_x with
    | true, true -> KB.Order.EQ
    | true, false -> KB.Order.LT
    | false, true -> KB.Order.GT
    | false, false -> KB.Order.NC

let compare x y =
  match order x y with
  | KB.Order.EQ -> 0
  | KB.Order.LT -> -1
  | KB.Order.GT -> 1
  | KB.Order.NC -> failwith "non-total order"

let contains x y =
  match order x y with
  | KB.Order.LT | KB.Order.EQ -> true
  | _ -> false

let join x y =
  let bw = Int.max x.bw y.bw in
  let m = mm bw in
  let abstract = Bitvec.((x.abs lor y.abs) mod m) in
  let just_conc = Bitvec.((x.conc lor y.conc) mod m) in
  let conc = Bitvec.((((lnot abstract) mod m) land just_conc) mod m) in
  { bw; conc; abs = abstract }

let meet x y =
  printf "Abstract_bitvector.meet not implemented";
  let max_bw = Int.max x.bw y.bw in
  make_top max_bw false
    
(** Conversions *)
let to_string {bw;conc;abs} =
  let bw = Int.to_string bw in
  let conc = Bitvec.to_string conc in
  let abs = Bitvec.to_string abs in
  Format.sprintf "[%s]<%s, %s>" bw conc abs
    
let of_word w =
  let bw = Word.bitwidth w in
  { bw; conc = Word.to_bitvec w; abs = z }

let of_z ?(width : int = 512) b =
  let m = mm width in
  let conc = (Bitvec.bigint b) mod m in
  { bw = width; conc; abs = z }

let of_int ?(width : int = 512) i =
  let m = mm width in
  let conc = (Bitvec.int i) mod m in
  { bw = width; conc; abs = z }

let sexp_of_t (x : t) =
  if is_bot x
  then Sexp.Atom "_|_"
  else if is_top x 
  then Sexp.Atom "T"
  else Sexp.List [Int.sexp_of_t x.bw;
                  Sexp.Atom (Bitvec.to_string x.conc);
                  Sexp.Atom (Bitvec.to_string x.abs)]

(** operations *)
let bitwidth { bw; _ } = bw

let unimplemented op = fun x y ->
  Format.printf "Abstract.bitvector.%s unimplemented" op;
  top

let unop_unimplemented op = fun x ->
  Format.printf "Abstract.bitvector.%s unimplemented" op;
  make_top x.bw false

(* most of these are implemented using the tristate num operations
   from the paper:
     ``Sound, Precise, and Fast Abstract Interpretation with Tristate
       Numbers''
   and from the linux tnum.c source *)
let add x y =
  let open Bitvec in
  let max_bw = Int.max x.bw y.bw in
  let bw = mm max_bw in
  let v = add x.conc y.conc mod bw in
  let m = add x.abs y.abs mod bw in
  let eps = add v m mod bw in
  let chi = logxor eps v mod bw in
  let eta = logor (logor chi y.abs mod bw) x.abs mod bw in
  let conc = logand v (lnot eta mod bw) mod bw in
  { bw = max_bw; conc; abs = eta }
    
let sub x y =
  let open Bitvec in
  let bw = Int.max x.bw y.bw in
  let m = mm bw in
  let dv = sub x.conc y.conc mod m in
  let alpha = add dv x.abs mod m in
  let beta = sub dv y.abs mod m in
  let chi = logxor alpha beta mod m in
  let mu = logor (logor chi x.abs mod m) y.abs mod m in
  let conc = logand dv (lnot mu mod m) mod m in
  { bw; conc; abs = mu }
  
let lshift = unimplemented "lshift"
let rshift = unimplemented "rshift"
let logand = unimplemented "logand"
let logor = unimplemented "logor"
let logxor = unimplemented "logxor"
let neg = unop_unimplemented "neg"
let lnot = unop_unimplemented "lnot"
let extract x hi lo =
  printf "Abstract_bitvector.extract unimplemented";
  make_top (hi - lo + 1) false
let concat = unimplemented "concat"

let mul = unimplemented "mul"
let div = unimplemented "div" 
let sdiv = unimplemented "sdiv"
let umod = unimplemented "umod"
let smod = unimplemented "smod"
let arshift = unimplemented "arshift"

let booleq = unimplemented "booleq"
let boolneq = unimplemented "boolneq"

let boollt = unimplemented "boollt"
let boolle = unimplemented "boolle"
let boolslt = unimplemented "boolslt"
let boolsle = unimplemented "boolsle"

let could_be_true _ =
  printf "Abstract_bitvector.could_be_true unimplemented";
  true
let could_be_false _ =
  printf "Abstract_bitvector.could_be_false unimplemented";
  true

let unsigned n _ =
  printf "Abstract_bitvector.unsigned unimplemented";
  make_top n false
let signed n _ =
  printf "Abstract_bitvector.signed unimplemented";
  make_top n false
let low n _ =
  printf "Abstract_bitvector.low unimplemented";
  make_top n false
let high n _ =
  printf "Abstract_bitvector.high unimplemented";
  make_top n false

(** As constituent of product domain *)
module Key = Domain_key.DomainKey

let key : t Key.k = Key.create "abstract-bitvector"
                      
let get : type a. a Key.k -> (t -> a) option = fun k ->
  match Key.eq_type k key with
  | Eq -> Some (fun x -> x)
  | Neq -> None

let set : type a. a Key.k -> t -> a -> t = fun k other replace ->
  match Key.eq_type k key with
  | Eq -> replace
  | Neq -> other
