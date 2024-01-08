open Core_kernel
open Bap_main
open Bap.Std
open Graphlib.Std
module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

open Common
open Abstract
open Abstract_memory

module Cfg = Bap.Std.Graphs.Cfg
module IrCfg = Bap.Std.Graphs.Ir
module CR = Common.CalleeRel
module CRS = CR.Set
module ABI = Common.AMD64SystemVABI

module ProdIntvlxTaint = DomainProduct(Wrapping_interval)(Checker_taint.Analysis)
module WithTypes = DomainProduct(ProdIntvlxTaint)(Type_domain)
(* module WithBitvectors = DomainProduct(WithTypes)(Abstract_bitvector) *)
module FinalDomain : Numeric_domain.Sig = DomainProduct(WithTypes)(Bases_domain)

module E = Abstract_memory.Make(FinalDomain)
module R = Region
module Rt = Region.Set
module Vt = struct type t = Common.cell_t end
module AbsInt = AbstractInterpreter(FinalDomain)(R)(Rt)(Vt)(E)

module TraceAbsInt = Trace.AbsInt.Make(FinalDomain)(E)
module TraceDir = Trace.Directives(FinalDomain)(E)
module TraceEnv = Trace.Env(FinalDomain)(E)

module HcCompute = Hc.Compute(FinalDomain)(E)

module Narrower = Uc_fixpoint.SingleShotRoundRobinNarrow(FinalDomain)
module WidenSetCompute = Uc_fixpoint.WidenSet

module OcData(Args : sig
    val dmap : TraceDir.directive_map
    val tidmap : Blk.elt Tid.Map.t
    val firstnode : Tid.t
    val widenset : Tid.Set.t
  end) : sig
  type data = TraceEnv.t
  type edge = Uc_graph_builder.UcOcamlG.T.E.t
  type vertex = Tid.t
  type g = Uc_graph_builder.UcOcamlG.T.t

  val direction : Uc_graph_builder.OcamlG.Fixpoint.direction
  val join : data -> data -> data
  val equal : data -> data -> bool
  val analyze : edge -> data -> data
  (* val widening : t -> t -> t *)
end = struct
  type data = TraceEnv.t
  type edge = Uc_graph_builder.UcOcamlG.T.E.t
  type vertex = Tid.t
  type g = Uc_graph_builder.UcOcamlG.T.t

  let counts = ref Tid.Map.empty
  let direction = Uc_graph_builder.OcamlG.Fixpoint.Forward
  let join = TraceEnv.merge
  let equal = TraceEnv.equal
  let analyze ((from_, cnd, to_) : edge) oldst =
    L.debug "Denoting elt: %a" Tid.ppo from_;
    L.debug "Old state is:";
    TraceEnv.pp oldst;
    let elt = match Tid.Map.find Args.tidmap from_ with
        | Some elt -> elt
        | None -> failwith "tid not found" in
    let in_widen_set = Tid.Set.mem Args.widenset from_ in
    if in_widen_set
    then 
      let cnt = match Tid.Map.find !counts from_ with
        | None -> 0
        | Some n -> n in
      let oldst = if cnt >= Common.ai_widen_threshold
        then TraceEnv.widen from_ oldst
        else oldst in
      counts := Tid.Map.set !counts ~key:from_ ~data:1;
      TraceAbsInt.denote_elt Args.dmap elt oldst
    else
      TraceAbsInt.denote_elt Args.dmap elt oldst

  (* let widening oldst newst = *)
  (*   (\* to keep old stuff compatible with new stuff,  *)
  (*      reuse all  *)
  (*        val widening_with_step : int -> 'n -> t -> t -> t *)
  (*      with the step counter set past the widening delay *)
  (*      since the OcamlGraph lib should only calling this *)
  (*      `widening' function after the delay *\) *)
  (*   TraceEnv.widen_with_step Common.ai_widen_threshold Args.firstnode oldst newst *)
end

(** Single shot pass runner *)

(** Logging *)
module L = struct
  include Dolog.Log
  let log_prefix = sprintf "%s.driver" Common.package
  let () = set_prefix log_prefix
end

(** Driver types *)
type analysis_result = {
  callees : CRS.t;
  alerts : Alert.Set.t;
}

type exn += EntryNodeNotFound

let emp_analysis_result = {
  callees = CRS.empty;
  alerts = Alert.Set.empty
}

let insns_of_node n = Blk.elts @@ Graphs.Ir.Node.label n
let first_insn_of_blk b =
  let insns = Blk.elts b in
  match Seq.hd insns with
  | Some i -> i
  | None -> failwith "In first_insn_of_blk, couldn't find first insn"

let sub_of_tid_exn tid proj : sub Term.t =
  let prog = Project.program proj in
  let sub = Term.find sub_t prog tid in
  match sub with
  | Some sub -> sub
  | None ->
    let e = Format.sprintf
              "In sub_of_tid_exn, didn't find sub with tid %a in the program"
              Tid.pps tid
    in
    failwith e

(* this is for handling special cases ffrom heavily optimized
   assembly: the case where a function is really just a label
   for a jump to some other direct call--which seems
   like a common idiom in libsodium. in these cases, there
   is no analysis to be done, and the usual abstract interpretation
   using BAP won't work because what should the CFG edges for a
   single insn function look like? we could add an entry node
   and an exit node and in hind sight, i now get why textbooks
   have always recommended this.

   this is really messy atm, but can be cleaned up later if
   separating callee getting of indirect and direct branches
*)
let should_skip_analysis (edges : Uc_graph_builder.UcBapG.edges)
      (tidmap : Blk.elt Tid_map.t)
      (sub : sub term)
      (prog : Program.t) : analysis_result option =
  let has_no_edges = List.is_empty edges in
  let insns = Map.data tidmap in
  let has_one_insn =  insns |> List.length |> Int.equal 1 in
  if has_no_edges && has_one_insn
  then
    match List.hd_exn insns with
    | `Jmp j ->
      let totid = match Jmp.kind j with
        | Goto (Direct totid) -> Some totid
        | Call c ->
          let target = Call.target c in
          begin
            match target with
            | Direct totid -> Some totid
            | _ -> None
          end
        | _ -> None in
      begin
        match totid with
        | None -> failwith "todo"
        | Some totid ->
          let callee_rels = CRS.singleton {
            callee = totid ;
            caller = Term.tid sub ;
            callsite = Term.tid j }
          in
          Some {
            alerts = Alert.Set.empty;
            callees = callee_rels;
          }
      end
    | _ -> failwith "in should_skip_analysis, subroutine is single instruction but non jump instruction. this case is not yet handled." 
  else None

let do_ (type a) ~(if_ : bool) ~(default : a) (f : unit -> a) : a =
  if if_ then f () else default

let set_taint (prod : FinalDomain.t) : FinalDomain.t =
  FinalDomain.set Checker_taint.Analysis.key prod Checker_taint.Analysis.Taint

let unset_taint (prod : FinalDomain.t) : FinalDomain.t =
  FinalDomain.set Checker_taint.Analysis.key prod Checker_taint.Analysis.Notaint

let propagate_taint config projctxt proj : unit =
  let open Uc_inargs in
  let open Analysis_profiling in
  let get_cached_taint_results projctxt : String.Set.t String.Map.t option =
    match Common.get_taint_cache projctxt with
    | None -> None
    | Some filename ->
      try
        Some (Sexp.load_sexp ~strict:true filename
              |> String.Map.t_of_sexp String.Set.t_of_sexp)
      with
      | e ->
        L.error "Couldn't parse cached taint results from file %s : %s" filename @@ Exn.to_string e;
        None
  in
  let cache_taint_results projctxt (results : String.Set.t String.Map.t) : unit =
    match Common.get_taint_cache projctxt with
    | None -> ()
    | Some filename ->
      let sexp = String.Map.sexp_of_t String.Set.sexp_of_t results in
      Sexp.save_hum filename sexp
  in
  let collect_results : TaintSummary.t TaintContext.Map.t -> String.Set.t String.Map.t =
    TaintContext.Map.fold ~init:String.Map.empty
      ~f:(fun ~key ~data inargs ->
        String.Map.update inargs key.subname
          ~f:(function
            | None -> key.argvals
            | Some previnstate -> String.Set.union previnstate key.argvals))
  in
  let set_taint (finalres : String.Set.t String.Map.t) : unit =
    String.Map.iteri finalres ~f:(fun ~key ~data ->
      let subname = key in
      let tainted_args = data in
      L.debug "Sub %s has tainted args: %s" subname (String.Set.to_list tainted_args |> List.to_string ~f:Fn.id);
      Uc_preanalyses.set_tainted_args subname tainted_args)
  in
  let popwl (st : InterprocState.t)
    : TaintContext.t option * InterprocState.t =
    match TaintContext.Set.min_elt st.worklist with
    | (Some ctxt) as next ->
      next, {st with
             worklist = TaintContext.Set.remove st.worklist ctxt }
    | None -> None, st
  in
  let rec loop st =
    match popwl st with
    | Some ctxt, st ->
      
      let st = timed ctxt.subname InterprocTaintPropagation @@ fun () ->
        L.info "Propagating taint for %s" ctxt.subname;
        let _, st = Analyzer.analyze_ctxt proj st ctxt in
        L.info "Done propagating taint to %s's callers" ctxt.subname;
          st
      in
      loop st
    | None, st -> st.results
  in
  let run_analysis () : String.Set.t String.Map.t =
    L.info "Starting interproc taint propagation";
    let results = timed "InterprocTaint" InterprocTaintPropagation @@ fun () ->
      let init_st = InterprocState.init @@ InterprocState.empty () in
      let results = loop init_st in
      results
    in
    collect_results results
  in
  let subtaints = match Common.get_taint_cache projctxt with
    | None ->
      let subtaints = run_analysis () in
      cache_taint_results projctxt subtaints;
      subtaints
    | Some path -> match get_cached_taint_results projctxt with
      | Some cached_taint ->
        L.info "loaded taint cache from %s" path;
        cached_taint
      | None ->
        L.info "no taint cache at %s" path;
        let subtaints = run_analysis () in
        cache_taint_results projctxt subtaints;
        subtaints
  in
  set_taint subtaints;
  L.info "Finished interproc taint propagation"

let run_analyses sub proj ~(is_toplevel : bool)
      ~(bss_init_stores : Global_function_pointers.global_const_store list)
      ~(config : Config.t)
      (ctxt : Bap_main.ctxt)
      (dbgtids : String.Set.t) : analysis_result =
  let open Analysis_profiling in
  
  let subname = Sub.name sub in
  let subtid = Term.tid sub in
  L.info "Analyzing %s" subname;
  Uc_stats.AnalysisInfo.record_analyzed_sub subname subtid;
  
  let should_dump_bir = Extension.Configuration.get ctxt Common.debug_dump in
  do_ ~if_:should_dump_bir ~default:() (fun () ->
    L.debug "%a" Sub.ppo sub);
  
  let prog = Project.program proj in

  let edges = Uc_preanalyses.get_init_edges subname in
  let tidmap = Uc_preanalyses.get_tidmap subname in
  
  match should_skip_analysis edges tidmap sub prog with
  | Some res ->
    L.info "Skipping analysis of single jmp subroutine %s\n%!" subname;
    res
  | None ->
    let do_cs = Extension.Configuration.get ctxt Common.do_cs_checks_param in
    let do_ss = Extension.Configuration.get ctxt Common.do_ss_checks_param in
    let do_dmp = Extension.Configuration.get ctxt Common.do_dmp_checks_param in
    let is_dbg = Uc_log.is_dbg @@
      Extension.Configuration.get ctxt Common.global_log_level_param in

    let module G = Graphlib.Make(Tid)(Uc_graph_builder.ExpOpt) in
    let edges = Uc_preanalyses.get_final_edges subname in
    let cfg = Graphlib.create (module G) ~edges () in
    L.debug "First node is: %a" Tid.ppo Uc_graph_builder.entrytid;

    L.debug "Edges are:";
    List.iter edges ~f:(fun e ->
      printf "\t%s\n" @@ Uc_graph_builder.string_of_bapedge e
    );

    (* let oc_graph = Uc_graph_builder.UcOcamlG.of_bapg *)
    (*                  (module G) *)
    (*                  cfg *)
    (*                  (fun e -> (G.Edge.src e, G.Edge.dst e, G.Edge.label e)) in *)

    (* let graphviz_fname = subname ^ ".dot" in *)
    (* Out_channel.with_file graphviz_fname ~f:(fun outchnl -> *)
    (*   Uc_graph_builder.OcamlGraphWriter.output_graph outchnl oc_graph); *)

    L.debug "getting final liveness";
    let dataflow_liveness = Uc_preanalyses.get_final_liveness subname in
    L.debug "done getting final liveness";

    (* dmp checker specific *)
    let lahf_sahf = do_ ~if_:do_dmp ~default:Lahf_and_sahf.default @@ fun () ->
      L.info "Running lahf sahf analysis";
      timed subname LahfSahfAnalysis @@ fun () ->
      let res = Lahf_and_sahf.run_on_cfg (module G) cfg tidmap in
      L.info "Done running lahf sahf analysis";
      res
    in

    do_ ~if_:do_dmp ~default:() (fun () ->
      Dmp_helpers.find_smalloc proj;
      Dmp_helpers.print_smalloc_addrs ());

    (* set up initial solution *)
    L.info "setting up initial solution";
    let start = Analysis_profiling.record_start_time () in
    let empty = E.empty in
    let stack_addr = 0x7fff_fff0 in
    let free_vars = Sub.free_vars sub in
    let freenames = Set.to_list free_vars
                    |> List.map ~f:Var.name
    in
    let args = Term.enum arg_t sub in
    let argnames = Seq.map args
                     ~f:(fun a -> Arg.var a |> T.Var.name)
                   |> Seq.to_list
    in
    let taintedregs = Uc_preanalyses.get_tainted_args subname in
    let with_canary_set = E.set_stack_canary empty in
    let env_with_df_set = E.set "DF" FinalDomain.b0 with_canary_set in
    let env_with_rsp_set = match E.set_rsp stack_addr env_with_df_set with
      | Ok env' -> env'
      | Error e -> failwith @@ Error.to_string_hum e
    in
    (* e.g., filter out bap's 'mem' var and the result var
       commonly used in prog analysis *)
    let true_args = List.append argnames freenames
                    |> List.filter ~f:ABI.var_name_is_arg
    in
    let args_initd = List.fold true_args
                      ~init:env_with_rsp_set
                      ~f:(fun mem argname ->
                        E.init_arg ~name:argname config sub mem)
    in
    let true_args = String.Set.of_list true_args in
    let untaintregs = String.Set.diff true_args taintedregs in
    let final_env = String.Set.fold untaintregs
                      ~init:args_initd
                      ~f:(fun env reg ->
                        let v' = E.lookup reg env
                                 |> unset_taint in
                        E.set reg v' env)
    in
    let final_env = String.Set.fold taintedregs
                      ~init:final_env
                      ~f:(fun env reg ->
                        let v' = E.lookup reg env
                                 |> set_taint in
                        E.set reg v' env)
    in
    
    let stop = Analysis_profiling.record_stop_time start in
    Analysis_profiling.record_duration_for subname InitEnvSetup stop;
    L.debug "done setting up init env";

    let flagownership = Uc_preanalyses.get_flagownership subname in
    let reachingdefs = Uc_preanalyses.get_reachingdefs subname in
    let dmp_st = Uc_preanalyses.get_dmpst subname in

    let dmp_bt_guards = do_ ~if_:do_dmp ~default:Dmp_helpers.default_gmap @@ fun () ->
      
      timed subname DmpGuardPointAnalysis @@ fun () ->
      Dmp_helpers.FindSafePtrBitTestPass.get_guard_points
        dmp_st
        reachingdefs
        Reachingdefs.users_transitive_closure
    in
    
    let dmp_bt_set (tid : Tid.t) (st : TraceEnv.t) : TraceEnv.t =
      let open Abstract_bitvector in
      let tree = match Dmp_helpers.get_guard dmp_bt_guards tid with
        | Some guards ->
          L.debug "Found guard at tid %a" Tid.ppo tid;
          List.fold !guards ~init:st.tree
            ~f:(fun t {var;set} ->
              TraceEnv.Tree.map t ~f:(fun st ->
                let v = E.lookup var st in
                let bv = of_prod FinalDomain.get v in
                let f = if set then set_60_bit else clear_60_bit in
                let bv = f bv in
                let v = set_in_prod FinalDomain.set v bv in
                L.debug "%a bv: %s\n%!" Tid.ppo tid (to_string bv);
                E.set var v st))
        | None -> st.tree
      in
      { tree }
    in

    L.info "running trace part pre-analysis";
    let rpo_traversal = Graphlib.reverse_postorder_traverse
                          (module G)
                          ~start:Uc_graph_builder.entrytid
                          cfg in
    let cond_scrape_st = Trace.ConditionFinder.init
                           ~rpo_traversal
                           ~tidmap
                           ~reachingdefs in
    let live_flags = Trace.ConditionFinder.FlagScraper.get_live_flags
                       cond_scrape_st in
    let cmov_cnd_flags = List.filter live_flags ~f:(fun lf ->
      Trace.ConditionFinder.FlagScraper.flag_used_in_cmov lf cond_scrape_st) in

    do_ ~if_:is_dbg ~default:() (fun () ->
    List.iter cmov_cnd_flags ~f:(fun (tid, name) ->
      L.debug "cmov_cnd_flag: %s (%a)" name Tid.ppo tid));
  
    let cond_extractor_st = TraceDir.Extractor.init tidmap reachingdefs in
    let dirs = List.fold cmov_cnd_flags ~init:[] ~f:(fun dirs lf ->
      let split_dir = TraceDir.Extractor.get_conds_for_flag lf cond_extractor_st in
      let combine_dir = TraceDir.Extractor.get_merge_point_for_flag_dirs
                          cond_extractor_st lf split_dir in
      match combine_dir with
      | None ->
        L.warn "live flag <%a, %s> couldn't get both split and combine directives"
          Tid.ppo (fst lf) (snd lf);
        dirs
      | Some combine_dir -> split_dir :: combine_dir :: dirs)
    in
    L.debug "trace part directives are:";
    List.iter dirs ~f:(fun td ->
      L.debug "\t%s" @@ TraceDir.to_string td);
    let dirs = [] in
    let directive_map = TraceDir.Extractor.to_directive_map dirs in
    L.info "done running trace part pre-analysis";

    L.info "running trace part abstract interpreter";
    let start = Analysis_profiling.record_start_time () in

    let top_env = String.Set.fold ABI.callee_clobbered_regs
                    ~init:E.empty
                    ~f:(fun env reg ->
                      match ABI.size_of_var_name reg with
                      | Some sz ->
                        let top = FinalDomain.make_top sz false in
                        E.set reg top env
                      | None -> E.set reg FinalDomain.top env)
                  |> TraceEnv.default_with_env
    in

    let init_trace_env = TraceEnv.default_with_env final_env in
    let init_mapping = G.Node.Map.set
                         G.Node.Map.empty
                         ~key:Uc_graph_builder.entrytid
                         ~data:init_trace_env
                       |> G.Node.Map.set
                            ~key:Uc_graph_builder.false_node
                            ~data:top_env
    in
    let init_sol = Solution.create init_mapping TraceEnv.empty in

    (* Compute the widening set *)
    let doms = Graphlib.dominators (module G) cfg Uc_graph_builder.entrytid
    in
    let is_sdomd (n : Tid.t) ~(by : Tid.t) : bool =
      let n = G.Node.create n in
      let by = G.Node.create by in
      Tree.is_descendant_of doms ~parent:by n
    in
    Abstract.widen_set := Tid.Set.empty;
    List.iter edges ~f:(fun (from_, to_, _cnd) ->
      if is_sdomd from_ ~by:to_
      then Abstract.widen_set := Tid.Set.add !Abstract.widen_set to_
    );
    L.debug "Widening set:";
    Tid.Set.iter !Abstract.widen_set
      ~f:(L.debug "\t%a" Tid.ppo);

    (* get the kill_after_vars map for a hacked up 
       sparse analysis *)
    (* let kill_afters = Uc_preanalyses.get_kill_after_vars subname in *)

    (* Run the main value-set analysis *)
    let interp =
      fun tid st ->
        let elt = match Tid.Map.find tidmap tid with
          | Some elt -> elt
          | None ->
            failwith @@
            sprintf
              "in calculating analysis_results, couldn't find tid %a in tidmap"
              Tid.pps tid
        in
        (* L.debug "denoting elt %a" Tid.ppo tid; *)
        (* L.debug "in env:"; TraceEnv.pp st; *)
        let st = dmp_bt_set tid st in
        (* let live_here = Liveness.liveness_at_tid dataflow_liveness tid in *)
        (* L.debug "liveness is:"; *)
        (* String.Set.iter live_here ~f:(L.debug "\t%s"); *)
        (* let st = TraceEnv.map st ~f:(fun env -> *)
        (*   E.filter env live_here) *)
        (* in *)
        (* L.debug "after liveness filter:"; *)
        (* TraceEnv.pp st; *)
        (* let () = *)
        (*   let tidstr = Format.sprintf "%a" Tid.pps tid in *)
        (*   let is_target = String.Set.mem dbgtids tidstr in *)
        (*   let is_target = true in *)
        (*   do_ ~if_:is_target ~default:() (fun () -> *)
        (*     L.debug "denoting elt %a%!" Tid.ppo tid; *)
        (*     TraceEnv.pp st); *)
        (* in *)
        TraceAbsInt.denote_elt directive_map elt st 
        (* let st = TraceAbsInt.denote_elt directive_map elt st in *)
        (* let killvars = Tid.Map.find kill_afters tid *)
        (*                |> Option.value ~default:String.Set.empty *)
        (* in *)
        (* String.Set.fold killvars ~init:st *)
        (*   ~f:(fun st kill -> *)
        (*     L.debug "unsetting %s" kill; *)
        (*     TraceEnv.map st ~f:(E.unset kill)) *)
    in

    let analysis_results = Graphlib.fixpoint
                             (module G)
                             cfg
                             ~step:TraceEnv.widen_with_step
                             ~init:init_sol
                             ~equal:TraceEnv.equal
                             ~merge:TraceEnv.merge
                             ~start:Uc_graph_builder.entrytid
                             ~f:interp
    in

    (* let analysis_results = Narrower.compute *)
    (*                          (module G) *)
    (*                          cfg *)
    (*                          ~initsol:analysis_results *)
    (*                          ~tidmap *)
    (*                          ~start:first_node *)
    (*                          ~f:interp *)
    (*                          ~merge:(TraceEnv.merge (\* ~meet:true *\)) *)
    (*                          ~default:TraceEnv.empty *)
    (*                          ~d_equal:TraceEnv.equal in *)

    L.info "done running trace part abstract interpreter\n%!";
    let stop = Analysis_profiling.record_stop_time start in
    Analysis_profiling.record_duration_for subname AbsInt stop;

    let no_symex = Extension.Configuration.get ctxt Common.no_symex_param in
    let use_symex = not no_symex in
    let symex_profiling_out_file = Extension.Configuration.get ctxt Common.symex_profiling_output_file_path_param in

    let module TracePartCheckerOracle
      : Common.CheckerInterp with type t := FinalDomain.t =
    struct
      let denote_exp (tid : tid) (exp : Bil.exp) : FinalDomain.t list =
        (* let in_state = CI.Chao.M.find tid analysis_results in *)
        (* let in_state = analysis_results tid in *)
        let in_state = Solution.get analysis_results tid in
        (* let in_state = Map.find_exn !tid_level_data tid in *)
        TraceAbsInt.nondet_denote_exp exp in_state
    end
    in

    let module CompSimpChecker = Comp_simp.Checker(FinalDomain)(TracePartCheckerOracle) in
    let module SSChecker = Silent_stores.Checker(FinalDomain)(TracePartCheckerOracle) in
    let module DmpChecker = Dmp.Checker(FinalDomain)(TracePartCheckerOracle) in

    let elt_of_tid tid = match Tid_map.find tidmap tid with
      | Some elt -> elt
      | None ->
        failwith @@ sprintf "In running checker on sub %s, couldn't find tid %a"
                      subname Tid.pps tid
    in

    let defs = ref None in
    let emp = Alert.Set.empty in
    let idx_st = Uc_preanalyses.get_idxst subname in

    L.info "Running checkers";
    L.info "SS: %B, CS: %B, DMP: %B" do_ss do_cs do_dmp;
    let all_alerts = timed subname CsChecking @@ fun () ->
      List.fold edges ~init:emp ~f:(fun alerts (_, to_tid, _) ->
        let elt = elt_of_tid to_tid in
        let cs_chkr_res = do_ ~if_:do_cs ~default:emp (fun () ->
          CompSimpChecker.check_elt subname to_tid elt)
        in
        let ss_chkr_res = do_ ~if_:do_ss ~default:emp (fun () ->
          SSChecker.check_elt use_symex sub to_tid idx_st
            defs symex_profiling_out_file reachingdefs tidmap elt) 
        in
        let dmp_chkr_res = do_ ~if_:do_dmp ~default:emp (fun () ->
          DmpChecker.check_elt sub to_tid lahf_sahf elt)
        in
        Set.union alerts cs_chkr_res
        |> Set.union ss_chkr_res
        |> Set.union dmp_chkr_res)
    in
    L.info "Done running checkers";
    
    L.info "Running insn idx filler";
    let all_alerts = timed subname AlertIdxFilling @@ fun () ->
      Alert.InsnIdxFiller.set_for_alert_set idx_st all_alerts
    in
    L.info "Done running insn idx filler";

    (* this is really dependency analysis info, not liveness info *)
    L.info "Running flags live out filler";
    let all_alerts = timed subname AlertDependencyFilling @@ fun () ->
      Alert.FlagsLiveOutFiller.set_for_alert_set tidmap flagownership reachingdefs all_alerts
    in
    L.info "done running flags live out filler";

    L.info "Setting dataflow liveness in alerts";
    let all_alerts = timed subname AlertLivenessFilling @@ fun () ->
      Alert.DataflowLivenessFiller.set_for_alert_set all_alerts dataflow_liveness
    in
    L.info "Done setting dataflow liveness in alerts";

    L.info "Getting callees for analysis";
    let start = Analysis_profiling.record_start_time () in

    let module GetCallees = Callees.Getter(FinalDomain) in
    let eval_indirect_exp : tid -> Bil.exp -> FinalDomain.t list = fun tid exp ->
      let env = Solution.get analysis_results tid in
      (* let env = analysis_results tid in *)
      (* let env = CI.Chao.M.find tid analysis_results in *)
      (* let env = Map.find_exn !tid_level_data tid in  *)
      TraceAbsInt.nondet_denote_exp exp env
    in

    let callee_analysis_results = GetCallees.get sub proj eval_indirect_exp in
    let callees = List.filter callee_analysis_results ~f:Or_error.is_ok
                  |> List.map ~f:Or_error.ok_exn
                  |> CalleeRel.Set.of_list
    in
    let stop = Analysis_profiling.record_stop_time start in

    Analysis_profiling.record_duration_for subname CalleeAnalysis stop;
    L.info "Callee analysis finished";
    { alerts = all_alerts;
      callees = callees; }
    
let check_config config ctxt proj : unit =
  Printexc.record_backtrace true;
  Random.self_init ();

  let target_fns = Config.get_target_fns_exn config proj in
  Uc_inargs.TaintInState.config (module ABI) config;
  let worklist = Sub.Set.of_list @@ Sequence.to_list target_fns in
  let processed = Sub.Set.empty in
  let init_res = Alert.Set.empty in

  Uc_preanalyses.register_preanalyses proj;
  Sub.Set.iter worklist ~f:Uc_preanalyses.init;

  L.debug "Entry node is: %a -- %a"
    Tid.ppo Uc_graph_builder.entrytid
    Def.ppo Uc_graph_builder.entry;
  L.debug "Exit node is: %a -- %a"
    Tid.ppo Uc_graph_builder.exittid
    Def.ppo Uc_graph_builder.exit;
  
  propagate_taint config ctxt proj;

  let debugtids = match Extension.Configuration.get ctxt Common.debug_tids_param with
    | Some file ->
      In_channel.with_file file ~f:(fun ch ->
        In_channel.input_lines ch
        |> String.Set.of_list)
    | None -> String.Set.empty
  in
  
  let global_store_data = Global_function_pointers.Libsodium.Analysis.get_all_init_fn_ptr_data ctxt proj in
  L.debug "Global stores are:";
  List.iter global_store_data ~f:(fun {data;addr} ->
    L.debug "mem[%a] <- %a" Word.ppo addr Word.ppo data);

  let should_dump_kb = Extension.Configuration.get ctxt Common.debug_dump in
  do_ ~if_:should_dump_kb ~default:() (fun () ->
    Format.printf "%a\n%!" KB.pp_state @@ Toplevel.current ());

  let rec loop ~(worklist : Sub.Set.t)
            ~(processed : Sub.Set.t)
            ~(res : Alert.Set.t)
            ~(callees : CRS.t)
            ~(is_toplevel : bool)
            ~(config : Config.t) : analysis_result =
    let worklist_wo_procd = Set.diff worklist processed in
    if Sub.Set.is_empty worklist_wo_procd
    then { alerts = res; callees } 
    else
      let sub = Set.min_elt_exn worklist_wo_procd in
      let worklist_wo_procd_wo_sub = Set.remove worklist_wo_procd sub in
      let next_processed = Set.add processed sub in
      L.info "processing sub %s (%a)" (Sub.name sub) Tid.ppo (Term.tid sub);
      if AnalysisBlackList.sub_is_blacklisted sub ||
         AnalysisBlackList.sub_is_not_linked sub
      then begin
        L.info "Skipping sub %s: blacklisted or not linked" (Sub.name sub);
        loop ~worklist:worklist_wo_procd_wo_sub
          ~processed:next_processed
          ~res
          ~callees
          ~is_toplevel:false
          ~config
      end
      else begin
        Uc_preanalyses.init sub;
        let current_res = run_analyses sub proj ctxt debugtids
                            ~is_toplevel
                            ~bss_init_stores:global_store_data
                            ~config
        in
        let callee_subs = CRS.to_list current_res.callees
                          |> List.map ~f:(fun (r : CR.t) -> sub_of_tid_exn r.callee proj)
                          |> Sub.Set.of_list in
        Sub.Set.iter callee_subs ~f:(fun callee ->
          L.info "CallOf: (%s, %s)\n%!" (Sub.name sub) (Sub.name callee));
        let next_worklist = Sub.Set.union worklist_wo_procd_wo_sub callee_subs in
        let all_alerts = Alert.Set.union res current_res.alerts in
        loop ~worklist:next_worklist
          ~processed:next_processed
          ~res:all_alerts
          ~callees:current_res.callees
          ~config
          ~is_toplevel:false
      end
  in
  (* Run the analyses and checkers *)
  let analysis_results = loop ~worklist
                           ~processed
                           ~callees:CRS.empty
                           ~res:init_res
                           ~is_toplevel:true
                           ~config
  in 
  (* post-processing *)
  let all_alerts = analysis_results.alerts in
  let all_alerts = Alert.OpcodeAndAddrFiller.set_for_alert_set all_alerts proj in
  let all_alerts = Alert.SubNameResolverFiller.resolve_sub_names all_alerts proj in
  let all_alerts = Alert.SubNameResolverFiller.set_for_alert_set all_alerts proj in
  let all_alerts = Alert.RemoveAllEmptySubName.set_for_alert_set all_alerts proj in
  let all_alerts = Alert.RemoveSpuriousCompSimpAlerts.set_for_alert_set all_alerts proj in
  let all_alerts = Alert.RemoveAlertsForCallInsns.set_for_alert_set all_alerts proj in

  let is_double_check = Extension.Configuration.get ctxt Common.is_double_check in
  let all_alerts = do_ ~if_:(not is_double_check) ~default:all_alerts (fun () ->
    Alert.RemoveAndWarnEmptyInsnIdxAlerts.set_for_alert_set all_alerts proj)
  in

  let all_alerts = Alert.CombinedTransformFixerUpper.set_for_alert_set all_alerts proj in

  let res = Alert.RemoveUnsupportedMirOpcodes.set_for_alert_set all_alerts proj in
  let all_alerts = res.alerts in
  
  L.info "Done processing all functions";
  
  Uc_stats.Eval.(info_print cs_stats "cs stats");
  Uc_stats.Eval.(info_print ss_stats "ss stats");
  Uc_stats.Eval.(info_print dmp_stats "dmp stats");

  let unsupported_count = res.num_removed in
  L.info "num alerts removed due to unsupported MIR opcodes: %d"
    unsupported_count;
  
  let csv_out_file_name = Extension.Configuration.get ctxt Common.output_csv_file_param in
  L.info "writing checker alerts to file: %s" csv_out_file_name;
  Alert.save_alerts_to_csv_file ~filename:csv_out_file_name all_alerts;
  Analysis_profiling.print_all_times ()
