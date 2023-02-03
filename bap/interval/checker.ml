open Core
open Bap.Std
open Graphlib.Std
open Common

module type S = sig
  type warns = Alert.Set.t
  type t = warns
  type env

  val check_elt : Blk.elt -> Live_variables.t -> env -> sub term -> project -> bool -> warns Common.checker_res 
  val name : string
end
