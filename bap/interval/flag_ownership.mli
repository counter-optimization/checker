open Core_kernel
open Bap.Std

type t

val equal : t -> t -> bool

val sexp_of_t : t -> Sexp.t

val empty : t 

val has_flags : t -> tid -> bool

val get_flags_of_def_tid : t -> tid -> Tid.Set.t

val defs_of_flags : t -> Tid.Set.t Tid.Map.t

module Pass : sig
  include Uc_single_shot_pass.PASS with type t = t
end
