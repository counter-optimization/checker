open Core_kernel
open Bap.Std

type t

(** takes a RPO traversal of Sub.to_cfg return (irg) *)
val analyze : 'a Seq.t -> ('a -> Blk.t) -> t

val tid_part_of_transform : t -> tid -> bool

val print : t -> unit
