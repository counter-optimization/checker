open Core_kernel
open Bap.Std
       
module KB = Bap_knowledge.Knowledge
module T = Bap_core_theory.Theory

module type S = sig
  type t
  val run : t -> Blk.elt -> t
end
