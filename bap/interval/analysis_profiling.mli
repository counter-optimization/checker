open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

type t

type funcname = String.t

type event = Edgebuilding
           | DependencyAnalysis
           | CfgCreation
           | InitEnvSetup
           | AbsInt
           | CsChecking
           | SsChecking
           | AlertIdxFilling
           | AlertDependencyFilling
           | AlertLivenessFilling
           | CalleeAnalysis
           | ClassicLiveness
           | NewDependenceAnalysis
           | None
[@@deriving equal]

val string_of_event : event -> string

val record_start_time : unit -> t

val record_stop_time : t -> t

val record_duration_for : funcname -> event -> t -> unit

val print_all_times : unit -> unit
