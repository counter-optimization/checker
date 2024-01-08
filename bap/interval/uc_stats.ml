open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

open KB.Syntax

let log_prefix = sprintf "%s.uc_stats" Common.package
module L = struct
  include Dolog.Log
  let () = set_prefix log_prefix
end            

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

module AnalysisInfo = struct
  type t

  type subname = string
    
  let subs_analyzed_cls : (t, unit) KB.Class.t =
    KB.Class.declare "subs-analyzed" () ~package

  let analyzed_sub_tid_slot =
    KB.Class.property ~package
      subs_analyzed_cls
      "analyzed-sub-tid"
      Common.tid_opt_domain

  let analyzed_sub_name_slot =
    KB.Class.property ~package
      subs_analyzed_cls
      "analyzed-sub-name-tid"
      Common.string_flat_dom
                                 
  let get_analyzed_subnames () : String.Set.t =
    let subnames = ref String.Set.empty in
    Toplevel.exec begin
      KB.objects subs_analyzed_cls >>= 
      KB.Seq.iter ~f:(fun v ->
        let* name = v-->analyzed_sub_name_slot in
        subnames := String.Set.add !subnames name;
        KB.return ())
    end;
    !subnames

  let record_analyzed_sub subname subtid : unit =
    Toplevel.exec begin
      KB.Object.create subs_analyzed_cls >>= fun obj ->
      KB.all_unit [
        KB.provide analyzed_sub_name_slot obj subname;
        KB.provide analyzed_sub_tid_slot obj (Some subtid)
      ]
    end
end

module Eval = struct
  type checker_stats

  (** classes *)
  let checker_stat_cls : (checker_stats, unit) KB.cls =
    KB.Class.declare "checker-stats" ()
      ~package
      ~public
      ~desc:"Class for holding checker stats"

  (** class slots *)
  type stat_type = (checker_stats, int) KB.slot
  let total =
    KB.Class.property
      checker_stat_cls
      "Total number of instructions considered"
      ~package
      ~public
      int_to_dom

  let bv_pruned =
    KB.Class.property
      checker_stat_cls
      "Instructions pruned by bv"
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

  let lahf_sahf_pruned =
    KB.Class.property
      checker_stat_cls
      "Dmp stores pruned due to being between LAHF-SAHF"
      ~package
      ~public
      int_to_dom

  (** KB symbols for interning *)
  type stat_category = string
  let dmp_stats = "dmp-stats"
  let cs_stats = "cs-stats"
  let ss_stats = "ss-stats"

  (** stats updating *)
  open KB.Syntax

  type t = (checker_stats, unit) KB.cls KB.value

  type count = int

  let get_obj stat =
    KB.Symbol.intern ~public ~package stat checker_stat_cls

  let get statcat =
    let obj = get_obj statcat in
    let kb = Toplevel.current () in
    match KB.run checker_stat_cls obj kb with
    | Ok (stat, _) -> stat
    | Error e -> failwith @@ KB.Conflict.to_string e

  let stat stats stattype =
    KB.Value.get stattype stats

  let incr (statcat : stat_category) (stattype : stat_type) : unit =
    Toplevel.exec begin
      let* stats = get_obj statcat in
      let* count = stats-->stattype in
      KB.provide stattype stats (count + 1)
    end

  let to_json_string stats =
    let s n v = sprintf "\"%s\" : \"%d\"" n v in
    let print_order = ["total_considered", total;
                       "bv_pruned", bv_pruned;
                       "taint_pruned", taint_pruned;
                       "interval_pruned", interval_pruned;
                       "interproc_pruned", interproc_pruned;
                       "symex_pruned", symex_pruned;
                       "unsupported_pruned", unsupported_pruned;
                       "lahf_sahf_pruned", lahf_sahf_pruned]
    in
    let json_body = List.map print_order
                      ~f:(fun (n, v) -> s n @@ stat stats v)
                    |> String.concat ~sep:"\n"
    in
    sprintf "{\n%s}\n" json_body

  let info_print statcat headermsg =
    L.info "%s:" headermsg;
    let stats = get statcat in
    L.info "%s" (to_json_string stats);
end
