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
val taint_pruned : stat_type
val interval_pruned : stat_type
val interproc_pruned : stat_type
val symex_pruned : stat_type
val unsupported_pruned : stat_type

