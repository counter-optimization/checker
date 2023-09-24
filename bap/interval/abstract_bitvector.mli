open Core_kernel
open Bap.Std

module KB = Bap_knowledge.Knowledge

val package : string

type t

val equal : t -> t -> bool
val bw_equal : t -> t -> bool
val order : t -> t -> KB.Order.partial
val compare : t -> t -> int
val contains : t -> t -> bool
val join : t -> t -> t
val meet : t -> t -> t
  
val to_string : t -> string
val of_word : word -> t
val of_z : ?width:int -> Z.t -> t
val of_int : ?width:int -> int -> t
val sexp_of_t : t -> Sexp.t

val make_top : int -> bool -> t
val make_bot : int -> bool -> t
val top : t
val bot : t
val one : int -> t
val b1 : t
val b0 : t
val is_top : t -> bool
val is_bot : t -> bool

val bitwidth : t -> int
val add : t -> t -> t
val sub : t -> t -> t
val lshift : t -> t -> t
val rshift : t -> t -> t
val logand : t -> t -> t
val logor : t -> t -> t
val logxor : t -> t -> t
val neg : t -> t
val lnot : t -> t
val extract : t -> int -> int -> t
val concat : t -> t -> t

val mul : t -> t -> t
val div : t -> t -> t
val sdiv : t -> t -> t
val umod : t -> t -> t
val smod : t -> t -> t
val arshift : t -> t -> t

val booleq : t -> t -> t
val boolneq : t -> t -> t
val boollt : t -> t -> t
val boolle : t -> t -> t
val boolslt : t -> t -> t
val boolsle : t -> t -> t

val could_be_true : t -> bool
val could_be_false : t -> bool

val unsigned : int -> t -> t
val signed : int -> t -> t
val low : int -> t -> t
val high : int -> t -> t

val key : t Domain_key.DomainKey.k
val get : 'a Domain_key.DomainKey.k -> (t -> 'a) option
val set : 'a Domain_key.DomainKey.k -> t -> 'a -> t


  
