open Core_kernel
open Bap.Std
open Graphlib.Std
open Common

type env = String.Set.t

type t

val sexp_of_t : t -> Sexp.t

val equal : t -> t -> bool
  
val run_on_cfg : (module Graph with type t = 'a and type node = Calling_context.t) -> 'a -> Blk.elt Tid.Map.t -> t

val liveness_at_tid : t -> tid -> env

val var_live_at_tid : t -> tid -> string -> bool

val enum : t -> (Calling_context.t * String.Set.t) Seq.t

val get_dead_defs : ?flagsonly:bool -> t -> Blk.elt Tid.Map.t -> Tidset.t
