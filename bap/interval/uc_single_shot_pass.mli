open Core_kernel
open Bap.Std

module KB = Bap_knowledge.Knowledge
module T = Bap_core_theory.Theory

type _ key = ..

module type PASS = sig
  type t
  type _ key += Key : t key
  val default : unit -> t
  val onjmp : jmp term -> t -> t
  val ondef : def term -> t -> t
  val onphi : phi term -> t -> t
end

module type GroupedAnalysesS = sig
  val register_runner : 'st. (module PASS with type t = 'st) -> unit
  val run : Blk.t Seq.t -> unit
  val get_final_state : 'a. (module PASS with type t='a) -> 'a
end

module GroupRunner : functor (Sizer : sig val n : int end) ->
sig
  include GroupedAnalysesS
end

val run_single : 'st. (module PASS with type t = 'st) -> Blk.t Seq.t -> 'st

module GroupedAnalyses : sig
  include GroupedAnalysesS
end
