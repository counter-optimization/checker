open Core_kernel
open Bap_main
open Bap.Std
open Graphlib.Std
module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

module Cfg = Bap.Std.Graphs.Cfg
module IrCfg = Bap.Std.Graphs.Ir

module G = Graphlib.Make(Calling_context)(Uc_graph_builder.ExpOpt)

(** Logging *)
let log_prefix = sprintf "%s.uc_preanalyses" Common.package
module L = struct
  include Dolog.Log
  let () = set_prefix log_prefix
end

open KB.Syntax
open Analysis_profiling

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
                       ~empty:Dmp_helpers.FindSafePtrBitTestPass.empty
                       "dmp_helper_st_dom"

let dmp_helper_slot = KB.Class.property ~public ~package
                        cls "dmp_helper-result"
                        dmp_helper_dom

let edges_dom = KB.Domain.flat
                  ~join:(fun l r -> Ok r)
                  ~inspect:(Uc_graph_builder.UcBapG.sexp_of_edges Uc_graph_builder.ExpOpt.sexp_of_t)
                  ~empty:[]
                  ~equal:(Uc_graph_builder.UcBapG.equal_edges Uc_graph_builder.ExpOpt.equal)
                  "edges-dom"
                  
let init_edges_slot = KB.Class.property ~public ~package
                        cls "init-edges"
                        edges_dom

let final_edges_slot = KB.Class.property ~public ~package
                         cls "final-edges"
                         edges_dom

let liveness_dom = KB.Domain.optional
                     ~inspect:Liveness.sexp_of_t
                     ~equal:Liveness.equal
                     "liveness-solution-dom"

let dfa_liveness_1_slot = KB.Class.property ~public ~package
                            cls "dfa-liveness-1"
                            liveness_dom

let dfa_liveness_2_slot = KB.Class.property ~public ~package
                            cls "dfa-liveness-2"
                            liveness_dom

let elt_to_sexp e =
  let l x = Sexp.List x in
  let a x = Sexp.Atom x in
  match e with
  | `Def d -> l [a "Def"; Def.sexp_of_t d]
  | `Phi p -> l [a "Phi"; Phi.sexp_of_t p]
  | `Jmp j -> l [a "Jmp"; Jmp.sexp_of_t j]

let elt_equal x y =
  match x, y with
  | `Def d1, `Def d2 -> Def.equal d1 d2
  | `Phi p1, `Phi p2 -> Phi.equal p1 p2
  | `Jmp j1, `Jmp j2 -> Jmp.equal j1 j2
  | _, _ -> false

let tidmap_dom = KB.Domain.flat
                   ~join:(fun l r -> Ok r)
                   ~inspect:(Tid.Map.sexp_of_t elt_to_sexp)
                   ~equal:(Tid.Map.equal elt_equal)
                   ~empty:Tid.Map.empty
                   "tidmap-dom"

let tidmap_slot = KB.Class.property ~public ~package
                    cls "tidmap"
                    tidmap_dom

let of_ (subname : string) : t KB.obj Bap_knowledge.knowledge =
  KB.Symbol.intern
    ~public:true
    ~package:Common.package
    subname
    cls

let get_final_edges (subname : string)
  : Uc_graph_builder.ExpOpt.t Uc_graph_builder.UcBapG.edges =
  Toplevel.eval final_edges_slot @@ of_ subname

let get_init_edges (subname : string)
  : Uc_graph_builder.ExpOpt.t Uc_graph_builder.UcBapG.edges =
  Toplevel.eval init_edges_slot @@ of_ subname

let get_cfg ?(init : bool = false) (subname : string)
  : G.t =
  let edges = if init
    then get_init_edges subname
    else get_final_edges subname in
  Graphlib.create (module G) ~edges ()

let get_tidmap (subname : string)
  : Blk.elt Tid.Map.t =
  Toplevel.eval tidmap_slot @@ of_ subname

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

let fill_single_shot_passes _proj =
  KB.observe subslot @@ fun obj sub ->
  KB.collect nameslot obj >>= fun subname ->
  L.debug "Filling idx_st,dmp_st,flagownership for %s" subname;
  let sub = match sub with
    | Some s -> s
    | None -> failwith "sub slot not filled in fill_single_shot_passes" in
  let idx_st, dmp_st, flagownership = timed subname GroupedSingleShotAnalyses @@ fun () ->
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
    idx_st, dmp_st, flagownership
  in
  KB.provide flag_ownership_slot obj flagownership >>= fun () ->
  KB.provide idx_st_slot obj idx_st >>= fun () ->
  KB.provide dmp_helper_slot obj dmp_st

let fill_edges proj =
  KB.observe idx_st_slot @@ fun obj idx_st ->
  KB.collect subslot obj >>= fun sub ->
  KB.collect nameslot obj >>= fun subname ->
  L.debug "Filling init edge and tidmap slots for %s" subname;
  let sub = match sub with
    | Some s -> s
    | None -> failwith "Sub not filled in fill_edges" in
  let edges, tidmap = timed subname Edgebuilding @@ fun () ->
    Uc_graph_builder.IntraNoResolve.of_sub_to_bapedges ~idxst:(Some idx_st) proj sub
  in
  KB.provide init_edges_slot obj edges >>= fun () ->
  KB.provide tidmap_slot obj tidmap 
  
let register_preanalyses proj =
  fill_single_shot_passes proj;
  fill_edges proj
  
      


