open Core_kernel
open Bap.Std
open Graphlib.Std

type varname = string

type defset

type sol

type t

val equal : t -> t -> bool

val compare : t -> t -> int

val sexp_of_t : t -> Sexp.t

val t_of_sexp : Sexp.t -> t

val join : t -> t -> t

val empty : t

val run_on_cfg : 'g. (module Graph with type t = 'g and type node = Tid.t) -> 'g -> sub term -> Blk.elt Tid_map.t -> Flag_ownership.t -> Tid.t -> t

val users_transitive_closure : t -> tid -> Tid.Set.t

(** Tidset because it could have 0, 1, or more possible def spots *)
val get_last_defs : t -> attid:tid -> forvar:varname -> Tid.Set.t

val has_users : t -> tid -> bool

val get_users : t -> tid -> Tid.Set.t

val get_uses : t -> tid -> Tid.Set.t
