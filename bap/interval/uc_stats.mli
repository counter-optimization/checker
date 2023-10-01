open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

(* val int_to_dom : int KB.Domain.t *)

(* type checker_stats *)

(* val checker_stat_cls : (checker_stats, unit) KB.cls *)

(* val  *)

type stat_category
val dmp_stats : stat_category
val cs_stats : stat_category
val ss_stats : stat_category

type stat_type
val total : stat_type
val bv_pruned : stat_type
val taint_pruned : stat_type
val interval_pruned : stat_type
val interproc_pruned : stat_type
val symex_pruned : stat_type
val unsupported_pruned : stat_type
val lahf_sahf_pruned : stat_type
  
type t
type count = int
val incr : stat_category -> stat_type -> unit
val get : stat_category -> t
val stat : t -> stat_type -> count
val to_json_string : t -> string
val info_print : stat_category -> string -> unit
