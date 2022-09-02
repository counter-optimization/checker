open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

open KB.Monad_infix

let package = "uarch-checker"
let cls_name = "analysis-results"

type t

let cls : (t, unit) KB.cls = KB.Class.declare ~package cls_name ()

let name_slot : (t, string) KB.slot =
  KB.Class.property
    ~package
    cls
    "fn-name"
    KB.Domain.string

(* let result_for_fn (fn : string) (s : KB.) = *)
(*   KB.Object.create cls >>= fun obj -> *)
  

