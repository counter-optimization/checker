open Core_kernel
open Bap_main
open Bap.Std
open Graphlib.Std
       
module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge
module Cfg = Bap.Std.Graphs.Cfg
module IrCfg = Bap.Std.Graphs.Ir
module G = Graphlib.Make(Tid)(Uc_graph_builder.ExpOpt)
module OG = Uc_graph_builder.UcOcamlG.T
module ABI = Abi.AMD64SystemVABI

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

let bool_dom = KB.Domain.total
                 ~inspect:Bool.sexp_of_t
                 ~join:(fun l r -> Ok (l || r))
                 ~empty:false
                 ~order:Bool.compare
                 "bool-domain"

let should_analyze_slot = KB.Class.property ~public ~package
                            cls "should-analyze?"
                            bool_dom

let first_node_slot = KB.Class.property ~public ~package
                        cls "first-node"
  @@ KB.Domain.optional ~equal:Tid.equal
       "first-node"

let tid_pset_dom = KB.Domain.powerset
                     (module Tid)
                     ~inspect:Tid.sexp_of_t
                     "tid-pset-dom"

let exit_nodes_slot = KB.Class.property ~public ~package
                        cls "exit-nodes"
                        tid_pset_dom

let tainted_args_dom = KB.Domain.total
                         ~inspect:String.Set.sexp_of_t
                         ~join:(fun l r ->
                           Ok (String.Set.inter l r))
                         ~empty:(ABI.gpr_arg_names
                                 |> String.Set.of_list)
                         ~order:(fun l r ->
                           match String.Set.compare l r with
                           | -1 -> 1
                           | 1 -> -1
                           | x -> x)
                         "inverse-string-pset-dom"

let tainted_args = KB.Class.property ~public ~package
                     cls "tainted-args" tainted_args_dom

let reachingdefs_dom = Reachingdefs.(KB.Domain.flat
                                       ~inspect:sexp_of_t
                                       ~join:(fun l r ->
                                         Ok (join l r))
                                           ~empty
                                           ~equal
                                           "reachingdefs-dom")

let reachingdefs = KB.Class.property ~public ~package
                     cls "reaching-defs" reachingdefs_dom

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

let og_cfg_of_edges ?(rev : bool = false) edges : OG.t =
  let nedges = List.length edges in
  let g = OG.create ~size:nedges () in
  List.iter edges ~f:(fun (from_, to_, cnd) ->
    if rev
    then OG.add_edge_e g (to_, cnd, from_)
    else OG.add_edge_e g (from_, cnd, to_));
  g

let edge_values_of_cfg (cfg : G.t) : String.Set.t Tid.Map.t =
  let add_to_live (at : Tid.t)
        (cnd : Exp.t)
        (m : String.Set.t Tid.Map.t) : String.Set.t Tid.Map.t =
    let cnd = Var_name_collector.run cnd in
    Tid.Map.update m at ~f:(function
      | Some prevexps -> String.Set.union prevexps cnd
      | None -> cnd)
  in
  G.edges cfg
  |> Seq.fold
       ~init:Tid.Map.empty
       ~f:(fun cnd_map ed ->
         let from_ = G.Edge.src ed in
         let cnd = G.Edge.label ed in
         match cnd with
         | Some cnd ->
           add_to_live from_ cnd cnd_map
         | None -> cnd_map)

let cfg_of_edges edges : G.t =
  Graphlib.create (module G) ~edges ()

let get_init_liveness (subname : string) : Liveness.t =
  let l = Toplevel.eval dfa_liveness_1_slot @@ of_ subname in
  match l with
  | Some l -> l
  | None ->
    failwith @@
    sprintf "Init liveness not computed yet for %s" subname

let get_final_liveness (subname : string) : Liveness.t =
  let l = Toplevel.eval dfa_liveness_2_slot @@ of_ subname in
  match l with
  | Some l -> l
  | None ->
    failwith @@
    sprintf "Final liveness not computed yet for %s" subname

let get_flagownership (subname : string) : Flag_ownership.t =
  Toplevel.eval flag_ownership_slot @@ of_ subname

let get_dmpst (subname : string) : Dmp_helpers.FindSafePtrBitTestPass.t =
  Toplevel.eval dmp_helper_slot @@ of_ subname

let get_first_node (subname : string) : Tid.t =
  match Toplevel.eval first_node_slot @@ of_ subname with
  | Some fn -> fn
  | None ->
    failwith @@
    sprintf "First node not computed yet for %s" subname

let get_exit_nodes (subname : string) : Tid.Set.t =
  Toplevel.eval exit_nodes_slot @@ of_ subname

let get_tidmap (subname : string) : Blk.elt Tid.Map.t =
  Toplevel.eval tidmap_slot @@ of_ subname

let get_idxst (subname : string) : Idx_calculator.t =
  Toplevel.eval idx_st_slot @@ of_ subname

let get_reachingdefs (subname : string) : Reachingdefs.t =
  Toplevel.eval reachingdefs @@ of_ subname

let init (sub : sub term) : unit =
  let name = Sub.name sub in
  let register_sub = of_ name >>= fun obj ->
    KB.provide subslot obj (Some sub) >>= fun () ->
    KB.provide nameslot obj name in
  Toplevel.exec register_sub

let get_sub (subname : string) =
  KB.Symbol.intern ~public ~package subname cls

let set_tainted_args
      (subname : string)
      (targs : String.Set.t) : unit =
  let provide_args = 
    let* obj = of_ subname in
    KB.provide tainted_args obj targs in
  Toplevel.exec provide_args

let get_tainted_args (subname : string) : String.Set.t =
  Toplevel.eval tainted_args @@ of_ subname

let fill_first_node proj =
  let first_insn_tid sub =
    let open Option.Monad_infix in
    let bbs = Term.enum blk_t sub in
    Seq.hd bbs >>= fun bb ->
    let elts = Blk.elts bb in
    Seq.hd elts >>= fun first_elt ->
    Option.some @@ Common.elt_to_tid first_elt
  in
  KB.promise first_node_slot @@ fun obj ->
  let* subname = obj-->nameslot in
  L.info "Filling first node for %s" subname;
  let* sub = obj-->subslot in
  match sub with
  | None -> failwith "subslot not filled for fill_first_node"
  | Some s ->
    let tid = first_insn_tid s in
    L.info "Done filling first node";
    KB.return tid

let fill_single_shot_passes _proj =
  KB.promise idx_st_slot @@ fun obj ->
  L.info "getting sub";
  let* sub = obj-->subslot in
  let* subname = obj-->nameslot in
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
  KB.provide dmp_helper_slot obj dmp_st >>= fun () ->
  L.info "Done filling idx_st,dmp_st,flagownership";
  KB.return idx_st

let fill_dmp_st _proj =
  KB.promise dmp_helper_slot @@ fun obj ->
  let* _ = obj-->idx_st_slot in
  let* dmp_st = obj-->dmp_helper_slot in
  KB.return dmp_st

let fill_flagownership _proj =
  KB.promise flag_ownership_slot @@ fun obj ->
  let* _ = obj-->idx_st_slot in
  let* flagownership = obj-->flag_ownership_slot in
  KB.return flagownership

let fill_edges proj =
  KB.promise init_edges_slot @@ fun obj ->
  let* subname = obj-->nameslot in
  L.info "Filling init edge and tidmap slots for %s" subname;
  L.debug "getting sub";
  let* sub = obj-->subslot in
  let* idx_st = obj-->idx_st_slot in
  let sub = match sub with
    | Some s -> s
    | None -> failwith "Sub not filled in fill_edges" in
  let edges, tidmap = timed subname Edgebuilding @@ fun () ->
    Uc_graph_builder.IntraNoResolve.of_sub_to_bapedges ~idxst:(Some idx_st) proj sub
  in
  KB.provide tidmap_slot obj tidmap >>= fun () ->
  L.info "Done filling init edges";
  KB.return edges

(** This registers the dependency that init edges computation
    also fills the tidmap slot *)
let fill_tidmap proj =
  KB.promise tidmap_slot @@ fun obj ->
    let* _ = obj-->init_edges_slot in
    let* tidmap = obj-->tidmap_slot in
    KB.return tidmap

let fill_should_analyze proj =
  KB.promise should_analyze_slot @@ fun obj ->
  let* subname = obj-->nameslot in
  L.info "Filling should_analyze slot for %s" subname;
  let* tidmap = obj-->tidmap_slot in
  let* es = obj-->init_edges_slot in
  let has_no_edges = List.is_empty es in
  let insns = Map.data tidmap in
  let has_one_insn =  insns |> List.length |> Int.equal 1 in
  if has_no_edges && has_one_insn
  then begin
    (match List.hd_exn insns with
     | `Jmp j -> (match Jmp.dst j with
       | None -> ()
       | Some dst -> (match Jmp.resolve dst with
         | First tid -> (match Common.sub_of_tid proj tid with
           | Some sub -> init sub
           | None -> ())
         | _ -> ()))
     | _ -> ());
    L.info "Done filling should analyze";
    KB.return false
  end
  else
    (L.info "Done filling should analyze";
     KB.return true)

let fill_init_liveness proj =
  KB.promise dfa_liveness_1_slot @@ fun obj ->
  let* subname = obj-->nameslot in
  L.info "Filling init liveness for %s" subname;
  let* tidmap = obj-->tidmap_slot in
  let* edges = obj-->init_edges_slot in
  let cfg = cfg_of_edges edges in
  let edge_values = edge_values_of_cfg cfg in
  let liveness = timed subname ClassicLivenessOne @@ fun () ->
    Liveness.run_on_cfg (module G)
      cfg
      ~tidmap
      ~init:edge_values
  in
  L.info "Done filling DFA init liveness";
  KB.return (Some liveness)

let fill_final_edges proj =
  KB.promise final_edges_slot @@ fun obj ->
  let* subname = obj-->nameslot in
  L.info "Filling final edges for %s" subname;
  obj-->dfa_liveness_1_slot >>= fun initliveness ->
  let initliveness = match initliveness with
    | Some l -> l
    | None -> failwith "init liveness not computed yet" in
  let* initedges = obj-->init_edges_slot in
  let* tidmap = obj-->tidmap_slot in
  obj-->first_node_slot >>= fun first_node ->
  let first_node = match first_node with
    | Some fn -> fn
    | None -> failwith "first_node not computed yet" in
  (* todo: use succ/pred info that is already precomputed *)
  let remove_orphaned_nodes edges =
    let cfg = Graphlib.create (module G) ~edges () in
    let orphaned_nodes = G.nodes cfg
                           |> Seq.filter ~f:(fun n ->
                             let no_preds = G.Node.preds n cfg
                                            |> Seq.is_empty in
                             let is_start = Tid.equal n first_node in
                             no_preds && not is_start) in
    Seq.iter orphaned_nodes ~f:(
      L.warn "tid %a is orphaned" Tid.ppo
    );
    let orphaned_nodes = Seq.fold orphaned_nodes ~init:Tid.Set.empty
                           ~f:(fun all n -> Tid.Set.add all n) in
    let removed = ref false in
    let edges = List.filter edges ~f:(fun (from_, _, _) ->
      let keep = not @@ Tid.Set.mem orphaned_nodes from_ in
      (if not keep then removed := true);
      keep) in
    edges, !removed
  in
  let rec remove_all_orphans edges =
    let edges', changed = remove_orphaned_nodes edges in
    if changed
    then remove_all_orphans edges'
    else edges'
  in
  let finaledges, exit_nodes = timed subname RemoveDeadFlagDefs @@ fun () ->
    let dead_defs = Liveness.get_dead_defs initliveness tidmap in
    let edges = Edge_builder.remove_dead_defs
                  initedges
                  dead_defs
                |> remove_all_orphans in
    let cfg = Graphlib.create (module G) ~edges () in
    let exit_nodes = G.nodes cfg
                     |> Seq.filter ~f:(fun n ->
                       G.Node.succs n cfg |> Seq.is_empty) in
    
    edges, exit_nodes
  in
  let exit_nodes = Seq.to_list exit_nodes
                   |> List.map ~f:G.Node.label
                   |> Tid.Set.of_list in
  KB.provide exit_nodes_slot obj exit_nodes >>= fun () ->
  L.info "Done filling final edges";
  KB.return finaledges

let fill_final_liveness proj =
  KB.promise dfa_liveness_2_slot @@ fun obj ->
  let* subname = obj-->nameslot in
  L.info "Filling final DFA liveness analysis for %s" subname;
  let* tidmap = obj-->tidmap_slot in
  let* edges = obj-->final_edges_slot in
  let cfg = cfg_of_edges edges in
  let edge_values = edge_values_of_cfg cfg in
  let liveness2 = timed subname ClassicLivenessTwo @@ fun () ->
    Liveness.run_on_cfg (module G)
      cfg
      ~tidmap
      ~init:edge_values
  in
  L.info "Done filling final DFA liveness analysis";
  KB.return (Some liveness2)

let fill_reachingdefs (_proj : Project.t) : unit =
  KB.promise reachingdefs @@ fun obj ->
  let* subname = obj-->nameslot in
  L.info "Filling reaching defs for %s" subname;
  let* sub = obj-->subslot in
  let sub = match sub with
    | Some s -> s
    | None -> failwith "sub slot not filled" in
  let* tidmap = obj-->tidmap_slot in
  let* flagownership = obj-->flag_ownership_slot in
  let* first_node = obj-->first_node_slot in
  let first_node = match first_node with
    | Some fn -> fn
    | None -> failwith "first_node not filled" in
  let* final_edges = obj-->final_edges_slot in
  let cfg = cfg_of_edges final_edges in
  L.info "Running reaching defs and def-use analysis";
  let rds = timed subname ReachingDefs @@ fun () ->
    Reachingdefs.run_on_cfg (module G) cfg sub tidmap flagownership first_node
  in
  L.info "Done running reaching defs and def-use analysis";
  KB.return rds
  
let register_preanalyses (proj : Project.t) : unit =
  fill_single_shot_passes proj;
  fill_dmp_st proj;
  fill_flagownership proj;
  fill_first_node proj;
  fill_edges proj;
  fill_tidmap proj;
  fill_should_analyze proj;
  fill_init_liveness proj;
  fill_final_edges proj;
  fill_final_liveness proj;
  fill_reachingdefs proj
