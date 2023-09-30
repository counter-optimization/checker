open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

let int_to_dom = KB.Domain.total
                   ~inspect:Int.sexp_of_t
                   ~join:(fun x y ->
                     Ok (Int.max x y))
                   ~empty:0
                   ~order:Int.compare
                   "totally-ordered-int63s"

(** convenience *)
let public = true
let package = Common.package

(** classes *)
type checker_stats

let checker_stat_cls : (checker_stats, unit) KB.cls =
  KB.Class.declare "checker-stats" ()
    ~package
    ~public
    ~desc:"Class for holding checker stats"

(** class slots *)
type stat_type : (checker_stats, int) KB.slot
let total =
  KB.Class.property
    checker_stat_cls
    "Total number of instructions considered"
    ~package
    ~public
    int_to_dom

let taint_pruned =
  KB.Class.property
    checker_stat_cls
    "Instructions pruned by taint"
    ~package
    ~public
    int_to_dom

let interval_pruned =
  KB.Class.property
    checker_stat_cls
    "Instructions pruned by interval"
    ~package
    ~public
    int_to_dom

let interproc_pruned =
  KB.Class.property
    checker_stat_cls
    "Instructions pruned by interprocedural-ness"
    ~package
    ~public
    int_to_dom

let symex_pruned =
  KB.Class.property
    checker_stat_cls
    "Instructions pruned by symbolic execution"
    ~package
    ~public
    int_to_dom

let unsupported_pruned =
  KB.Class.property
    checker_stat_cls
    "Instructions pruned due to being unsupported"
    ~package
    ~public
    int_to_dom

(** KB symbols for interning *)
type stat_category = string
let dmp_stats = "dmp-stats"
let cs_stats = "cs-stats"
let ss_stats = "ss-stats"

(** stats updating *)
(* let incr symbol slot = *)
(*   KB. *)
  

