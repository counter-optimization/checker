open Core_kernel
open Bap.Std
open Graphlib.Std

type varname = string

type defset

type sol

type t

val run_on_cfg : 'g. (module Graph with type t = 'g and type node = Tid.t) -> 'g -> sub term -> Blk.elt Tid_map.t -> Flag_ownership.t -> Tid.t -> t

val users_transitive_closure : t -> tid -> Tidset.t

(** Tidset because it could have 0, 1, or more possible def spots *)
val get_last_defs : t -> attid:tid -> forvar:varname -> Tidset.t

val has_users : t -> tid -> bool

val get_users : t -> tid -> Tidset.t

val get_uses : t -> tid -> Tidset.t
