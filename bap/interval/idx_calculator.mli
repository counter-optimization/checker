open Core
open Bap.Std

type idx = int

type t

val build : sub term -> t

val contains_tid : tid -> t -> bool

val elt_is_idx_insn : Blk.elt -> bool

val is_part_of_idx_insn : t -> tid -> bool

val get_idx : tid -> t -> idx option
