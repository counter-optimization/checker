open Core_kernel
open Bap_main
open Bap.Std
open Graphlib.Std
module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

module Cfg = Bap.Std.Graphs.Cfg
module IrCfg = Bap.Std.Graphs.Ir

(** Logging *)
let log_prefix = sprintf "%s.uc_preanalyses" Common.package
module L = struct
  include Dolog.Log
  let () = set_prefix log_prefix
end

open KB.Syntax

let public = true
let package = Common.package

(** Current preanalyses: 
    * GroupedAnalyses:
      - Dmp_helpers.FindSafePtrBitTestPass
      - Idx_calculator.Pass
      - Flag_ownership.Pass
    * Uc_graph_builder.IntraNoResolve.of_sub_to_bapedges 
      (builds edges and tidmap)
      (depends on idx_st/Idx_calculator.Pass)
    * is jmp only sub
      (depends on edges and tidmap)
    * create CFG
      (depends on edges)
    * Liveness
      (depends on tidmap and cfg)
    * first node
    * remove dead defs
      (depends liveness, tidmap, edges, CFG)
    * rebuild edges
    * rebuild CFG
    * remove orphaned BBs/CFG nodes
    
*)

type t

let cls : (t, unit) KB.cls = KB.Class.declare
                               ~public:true
                               ~desc:"Holds analysis results and artifacts for each sub"
                               ~package:Common.package
                               "analysis-results"
                               ()

type exn += SubNotFoundToFillSlot of string
    
let subdom = KB.Domain.optional
               ~inspect:Sub.sexp_of_t
               ~join:(fun l r -> Ok r)
               ~equal:Sub.equal
               "sub-dom"
               
let subslot = KB.Class.property
                ~public:true
                ~package:Common.package
                cls
                "sub-slot"
                subdom

let namedom = KB.Domain.flat
                ~inspect:String.sexp_of_t
                ~join:(fun l r -> Ok r)
                ~empty:""
                ~equal:String.equal
                "string-dom"

let nameslot = KB.Class.property
                 ~public:true
                 ~package:Common.package
                 cls
                 "name-slot"
                 namedom

let flag_ownership_dom = KB.Domain.flat
                         ~join:(fun l r -> Ok r)
                         ~inspect:Flag_ownership.sexp_of_t
                         ~empty:Flag_ownership.empty
                         ~equal:Flag_ownership.equal
                         "per-sub-flag-ownership"

let flag_ownership_slot = KB.Class.property
                          ~public:true
                          ~package:Common.package
                          cls
                          "flag-ownership-result"
                          flag_ownership_dom

let idx_st_dom = KB.Domain.flat
                   ~join:(fun l r -> Ok (Idx_calculator.join l r))
                   ~inspect:Idx_calculator.sexp_of_t
                   ~empty:Idx_calculator.empty
                   ~equal:Idx_calculator.equal
                   "idx-calculation-result"


let idx_st_slot = KB.Class.property ~public ~package
                    cls "idx-calculator-result"
                    idx_st_dom

let dmp_helper_dom = KB.Domain.flat
                       ~join:(fun l r -> Ok r)
                       ~inspect:Dmp_helpers.FindSafePtrBitTestPass.sexp_of_t
                       ~equal:Dmp_helpers.FindSafePtrBitTestPass.equal
                       "dmp_helper_st_dom"

let dmp_helper_slot = KB.Class.property ~public ~package
                        cls "dmp_helper-result"
                        dmp_helper_dom
                       
                  

let of_ (subname : string) : t KB.obj Bap_knowledge.knowledge =
  KB.Symbol.intern
    ~public:true
    ~package:Common.package
    subname
    cls

let put_name (subname : string) : unit =
  Toplevel.exec begin
    of_ subname >>= fun obj ->
    KB.provide nameslot obj subname
  end

let put_sub (subname : string) (sub : sub term) : unit =
  Toplevel.exec begin
    of_ subname >>= fun obj ->
    KB.provide subslot obj (Some sub)
  end

let fill_single_shot_passes () =
  KB.observe subslot @@ fun obj sub ->
  let sub = match sub with
    | Some s -> s
    | None -> failwith "sub slot not filled in fill_single_shot_passes" in
  let tid_graph = Sub.to_graph sub in
  let irg = Sub.to_cfg sub in
  let irg_rpo = Graphlib.reverse_postorder_traverse
                  (module IrCfg) irg
                |> Seq.map ~f:IrCfg.Node.label in
  let succ t = Graphs.Tid.Node.succs t tid_graph in
  Uc_single_shot_pass.GroupedAnalyses.run irg_rpo;
  let idx_st = Uc_single_shot_pass.GroupedAnalyses.get_final_state
                 (module Idx_calculator.Pass)
             |> Idx_calculator.Pass.get_state ~succ in
  let dmp_st = Uc_single_shot_pass.GroupedAnalyses.get_final_state (module Dmp_helpers.FindSafePtrBitTestPass) in
  let flagownership = Uc_single_shot_pass.GroupedAnalyses.get_final_state (module Flag_ownership.Pass) in
  KB.provide flag_ownership_slot obj flagownership >>= fun () ->
  KB.provide idx_st_slot obj idx_st >>= fun () ->
  KB.provide dmp_helper_slot obj dmp_st

let () =
  fill_single_shot_passes ()
  
      


