open Core_kernel
open Bap.Std
open Graphlib.Std

type t

val default : t

(** takes a RPO traversal of Sub.to_cfg return (irg) *)
val run_on_cfg : 'g. (module Graph with type t = 'g and type node = Calling_context.t) -> 'g -> Blk.elt Tid_map.t -> t

val tid_part_of_transform : t -> tid -> bool

val print : t -> unit
