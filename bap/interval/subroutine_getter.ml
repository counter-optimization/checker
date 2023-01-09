open Core
open Bap.Std
open Graphlib.Std
open Common

module T = Bap_core_theory.Theory
 module KB = Bap_knowledge.Knowledge

type name = string
type addr = word

type t = (name, addr) List.Assoc.t

let _ =
  KB.

