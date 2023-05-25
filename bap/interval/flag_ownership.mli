open Core
open Bap.Std

type t = Set.M(Tid).t Tid_map.t

val run : unit -> t

val has_flags : t -> tid -> bool

val get_flags_of_def_tid : t -> tid -> Set.M(Tid).t
