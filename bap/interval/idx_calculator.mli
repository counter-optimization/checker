open Core_kernel
open Bap.Std

type idx = int

module T : sig
  type t
end

type t = T.t

module Pass : sig
  include Uc_single_shot_pass.PASS
  val get_state : succ:(tid -> tid Seq.t) -> t -> T.t
end

(* val build : sub term -> t *)

val contains_tid : tid -> t -> bool

(* val elt_is_idx_insn : Blk.elt -> bool *)

val is_part_of_idx_insn : t -> tid -> bool

val get_idx : tid -> t -> idx option
