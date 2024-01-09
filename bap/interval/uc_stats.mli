open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

module Eval : sig
  type stat_category
  val dmp_stats : stat_category
  val cs_stats : stat_category
  val ss_stats : stat_category
  val stat_category_equal : stat_category -> stat_category -> bool

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
  val get_count : stat_category -> stat_type -> int
  val stat : t -> stat_type -> count
  val to_json_string : t -> string
  val info_print : stat_category -> string -> unit
end

module AnalysisInfo : sig
  type t
  type subname = string
  val get_analyzed_subnames : unit -> String.Set.t
  val record_analyzed_sub : subname -> tid -> unit
end
