open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

open KB.Monad_infix

type t = {
  start : int;
  stop : int;
  duration : int
}

type funcname = String.t

type event = Edgebuilding
           | DependencyAnalysis
           | RemoveDeadFlagDefs
           | ReachingDefs
           | CfgCreation
           | InitEnvSetup
           | AbsInt
           | CsChecking
           | SsChecking
           | AlertIdxFilling
           | AlertDependencyFilling
           | AlertLivenessFilling
           | CalleeAnalysis
           | ClassicLivenessOne
           | ClassicLivenessTwo
           | NewDependenceAnalysis
           | LahfSahfAnalysis
           | DmpGuardPointAnalysis
           | GroupedSingleShotAnalyses
           | InterprocTaintPropagation
           | Default
[@@deriving equal]

let string_of_event = function
  | Edgebuilding -> "Edgebuilding"
  | DependencyAnalysis -> "DependencyAnalysis"
  | RemoveDeadFlagDefs -> "RemoveDeadFlagDefs"
  | ReachingDefs -> "ReachingDefs"
  | CfgCreation -> "CfgCreation"
  | InitEnvSetup -> "InitEnvSetup"
  | AbsInt -> "AbsInt"
  | CsChecking -> "CsChecking"
  | SsChecking -> "SsChecking"
  | AlertIdxFilling -> "AlertIdxFilling"
  | AlertDependencyFilling -> "AlertDependencyFilling"
  | AlertLivenessFilling -> "AlertLivenessFilling"
  | CalleeAnalysis -> "CalleeAnalysis"
  | ClassicLivenessOne -> "ClassicLivenessOne"
  | ClassicLivenessTwo -> "ClassicLivenessTwo"
  | NewDependenceAnalysis -> "NewDependenceAnalysis"
  | LahfSahfAnalysis -> "LahfSahfAnalysis"
  | DmpGuardPointAnalysis -> "DmpGuardPointAnalysis"
  | GroupedSingleShotAnalyses -> "GroupedSingleShotAnalyses"
  | InterprocTaintPropagation -> "InterprocTaintPropagation"
  | Default -> "Default"

let cls : (t, unit) KB.Class.t = KB.Class.declare
                                   ~package:Common.package
                                   "profiling-data"
                                   ()

let event_dom = KB.Domain.flat
                  ~empty:Default
                  ~equal:equal_event
                  "event dom"

let event_slot = KB.Class.property
                   ~package:Common.package
                   cls
                   "eventtype"
                   event_dom

let funcname_slot = KB.Class.property
                      ~package:Common.package
                      cls
                      "funcname"
                      Common.funcname_dom

let start_slot = KB.Class.property
                   ~package:Common.package
                   cls
                   "starttime_ns"
                   Common.int_total_order_dom

let stop_slot = KB.Class.property
                  ~package:Common.package
                  cls
                  "stoptime_ns"
                  Common.int_total_order_dom

let duration_slot = KB.Class.property
                      ~package:Common.package
                      cls
                      "duration_ns"
                      Common.int_total_order_dom

let record_start_time () : t =
  let start = Time_ns.now () |> Time_ns.to_int_ns_since_epoch in
  { start; stop = -1; duration = -1 }

let record_stop_time (r : t) : t =
  let stop = Time_ns.now () |> Time_ns.to_int_ns_since_epoch in
  let duration = stop - r.start in
  { r with stop; duration }

let record_duration_for (funcname : funcname)
      (event : event)
      (r : t) : unit =
  Toplevel.exec (KB.Object.create cls >>= fun obj ->
                 KB.all_unit [
                   KB.provide funcname_slot obj funcname;
                   KB.provide event_slot obj event;
                   KB.provide start_slot obj r.start;
                   KB.provide stop_slot obj r.stop;
                   KB.provide duration_slot obj r.duration;
                 ])

let timed (type a) subname event thunk : a =
  let start = record_start_time () in
  let res : a = thunk () in
  let stop = record_stop_time start in
  record_duration_for subname event stop;
  res

let print_prof_event (event : t KB.obj) : unit =
  Toplevel.exec begin
    KB.collect funcname_slot event >>= fun funcname ->
    KB.collect event_slot event >>= fun eventtype ->
    KB.collect duration_slot event >>= fun duration ->
    let eventtype = string_of_event eventtype in
    let duration = Int.to_string duration in 
    KB.return @@
    printf "%s, %s, %s\n%!" funcname eventtype duration
  end

let print_all_times () : unit =
  let funcname_grouper (event1 : t KB.obj) (event2 : t KB.obj) : bool =
    let is_equal = ref false in
    Toplevel.exec begin
      KB.collect funcname_slot event1 >>= fun funcname1 ->
      KB.collect funcname_slot event2 >>= fun funcname2 ->
      is_equal := String.equal funcname1 funcname2;
      KB.return ()
    end;
    !is_equal
  in
  let do_printing = begin
    KB.objects cls >>= fun prof_events ->
    let grouped_events = Seq.group prof_events ~break:funcname_grouper in
    let ungrouped_events = Seq.map grouped_events ~f:(Seq.of_list)
                           |> Seq.join
    in
    KB.return @@ Seq.iter ungrouped_events ~f:print_prof_event
  end
  in
  Toplevel.exec do_printing

