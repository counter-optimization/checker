open Core
open Bap.Std

type idx = int

type t

val build : sub term -> t

val contains_tid : tid -> t -> bool

val get_idx : tid -> t -> idx option
