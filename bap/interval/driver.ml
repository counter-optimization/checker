open Core
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
module FinalDomain = DomainProduct(WithTypes)(Bases_domain)

module E = Abstract_memory.Make(FinalDomain)
module R = Region
module Rt = Region.Set
module Vt = struct type t = Common.cell_t end
module AbsInt = AbstractInterpreter(FinalDomain)(R)(Rt)(Vt)(E)

module TraceAbsInt = Trace.AbsInt.Make(FinalDomain)(E)
module TraceDir = Trace.Directives(FinalDomain)(E)
module TraceEnv = Trace.Env(FinalDomain)(E)

type check_sub_result = {
    callees : CRS.t;
    liveness_info : Live_variables.t;
    csevalstats : EvalStats.t;
    ssevalstats : EvalStats.t;
    alerts : Alert.Set.t
}

type checker_alerts = Alert.Set.t

type analysis_result = {
    callees : CRS.t;
    alerts : checker_alerts;
    csevalstats : EvalStats.t;
    ssevalstats : EvalStats.t
  }

type t

module SubSet = struct
  include Set.Make_binable_using_comparator(Sub)
end

let subs_analyzed_cls : (t, unit) KB.Class.t = KB.Class.declare
                                                 "subs-analyzed"
                                                 ()
                                                 ~package:Common.package

let analyzed_sub_tid_slot = KB.Class.property
                              ~package:Common.package
                              subs_analyzed_cls
                              "analyzed-sub-tid"
                              Common.tid_opt_domain

let analyzed_sub_name_slot = KB.Class.property
                               ~package:Common.package
                               subs_analyzed_cls
                               "analyzed-sub-name-tid"
                               Common.string_flat_dom

let get_analyzed_subnames () : Set.M(String).t =
  let open KB.Monad_infix in
  let subnames = ref (Set.empty (module String)) in
  let () = Toplevel.exec begin
               KB.objects subs_analyzed_cls >>= fun analyzed ->
               KB.Seq.iter analyzed ~f:(fun v ->
                   KB.collect analyzed_sub_name_slot v >>= fun name ->
                   subnames := Set.add !subnames name;
                   KB.return ())
             end
  in
  !subnames

let record_analyzed_sub subname subtid : unit =
  let open KB.Monad_infix in
  Toplevel.exec begin
      KB.Object.create subs_analyzed_cls >>= fun obj ->
      KB.all_unit [
          KB.provide analyzed_sub_name_slot obj subname;
          KB.provide analyzed_sub_tid_slot obj (Some subtid)
        ]
    end

let insns_of_node n = Blk.elts @@ Graphs.Ir.Node.label n
let first_insn_of_blk b =
  let insns = Blk.elts b in
  match Seq.hd insns with
  | Some i -> i
  | None -> failwith "In first_insn_of_blk, couldn't find first insn"

let get_ret_insn_tid sub_nodes =
  let num = Seq.length sub_nodes in
  let last_node = Seq.nth_exn sub_nodes (num - 1) in
  let insns = insns_of_node last_node in
  let num_insns = Seq.length insns in
  let res =
    Seq.fold insns ~init:(None, 1) ~f:(fun (last, idx) insn ->
        let next_idx = idx + 1 in
        match idx, insn with
        | n, `Jmp j when n = num_insns -> Some (Term.tid j), next_idx
        | n, _ when n = num_insns -> failwith "Jmp/Ret was not last insn in sub"
        | _, _ -> None, next_idx)
  in
  match res with
  | Some tid, _ -> tid
  | _, _ -> failwith "Error finding last insn in sub"

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

let last_insn_of_sub sub : Blk.elt =
  let irg = Sub.to_cfg sub in
  let rev_nodes = Graphlib.postorder_traverse (module Graphs.Ir) irg in
  let last_node = match Seq.hd rev_nodes with
    | Some n -> n
    | None ->
       begin
         let sub_name = Sub.name sub in
         let err_s = sprintf "Couldn't get last node of sub %s in last_insns_of_sub" sub_name in
         failwith err_s
       end
  in
  let last_node_insns = insns_of_node last_node in
  let num_insns = Seq.length last_node_insns in
  Seq.nth_exn last_node_insns (num_insns - 1)

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
let should_skip_analysis (edges : Edge_builder.edges)
      (tidmap : Blk.elt Tid_map.t)
      (sub : sub term)
      (prog : Program.t) : check_sub_result option =
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
            let callee_rels = CRS.singleton { callee = totid ;
                                              caller = Term.tid sub ;
                                              callsite = Term.tid j } in
            let empty_alerts = Alert.Set.empty in
            Some { alerts = empty_alerts;
                   callees = callee_rels;
                   csevalstats = EvalStats.init;
                   ssevalstats = EvalStats.init;
                   liveness_info = Live_variables.IsUsedPass.UseRel.empty }
       end
       | _ -> failwith "in should_skip_analysis, subroutine is single instruction but non jump instruction. this case is not yet handled." 
  else
    None

let run_analyses sub img proj ~(is_toplevel : bool)
                 ~(bss_init_stores : Global_function_pointers.global_const_store list)
                 ~(config : Config.t)
                 ~(do_ss_checks : bool)
                 ~(do_cs_checks : bool)
                 ~(flagownership : Flag_ownership.t)
                 ctxt : check_sub_result =
  let subname = Sub.name sub in
  let subtid = Term.tid sub in
  let () = printf "Running analysis on sub %s\n%!" subname in
  let () = record_analyzed_sub subname subtid in
  let prog = Project.program proj in
  let idx_st = Idx_calculator.build sub in
  let start = Analysis_profiling.record_start_time () in
  let edges, tidmap = Edge_builder.run_one sub proj idx_st in
  let stop = Analysis_profiling.record_stop_time start in
  let () = Analysis_profiling.record_duration_for subname Edgebuilding stop in
  match should_skip_analysis edges tidmap sub prog with
  | Some res ->
     let sub_name = Sub.name sub in
     let () = Format.printf
                "Skipping analysis of single jmp subroutine %s\n%!"
                sub_name in
     res
  | None ->
     (* run the liveness analysis *)
     let () = printf "Running liveness analysis\n%!" in
     let start = Analysis_profiling.record_start_time () in
     let liveness = Live_variables.Analysis.run sub in
     let stop = Analysis_profiling.record_stop_time start in
     let () = Analysis_profiling.record_duration_for subname DependencyAnalysis stop in

     (* CFG *)
     let start = Analysis_profiling.record_start_time () in
     let edges = List.map edges ~f:(Calling_context.of_edge) in
     let module G = Graphlib.Make(Calling_context)(Bool) in
     let cfg = Graphlib.create (module G) ~edges () in
     let stop = Analysis_profiling.record_stop_time start in
     let () = Analysis_profiling.record_duration_for subname CfgCreation stop in

     (* set up initial solution *)
     let start = Analysis_profiling.record_start_time () in
     let () = printf "Setting up initial solution \n%!" in
     let empty = E.empty in
     let stack_addr = 0x7fff_fff0 in
     
     let free_vars = Sub.free_vars sub in
     let freenames = Set.to_list free_vars |> List.map ~f:Var.name in
     
     let args = Term.enum arg_t sub in
     let argnames = Seq.map args ~f:(fun a -> Arg.var a |> T.Var.name)
                    |> Seq.to_list in

     let with_canary_set = E.set_stack_canary empty in

     let env_with_df_set = E.set "DF" FinalDomain.b0 with_canary_set in
     let env_with_rsp_set = match E.set_rsp stack_addr env_with_df_set with
         | Ok env' -> env'
         | Error e -> failwith @@ Error.to_string_hum e
     in
     let env_with_img_set = E.set_img env_with_rsp_set img in
     (* e.g., filter out bap's 'mem' var and the result var
        commonly used in prog analysis *)
     let true_args = List.append argnames freenames
                     |> List.filter ~f:ABI.var_name_is_arg
     in
     let final_env = List.fold true_args
                               ~init:env_with_img_set
                               ~f:(fun mem argname ->
                                 E.init_arg ~name:argname config sub mem)
     in
     let rpo_traversal = Graphlib.reverse_postorder_traverse (module G) cfg in
     let po_traversal = Graphlib.postorder_traverse (module G) cfg in
     let first_node = match Seq.hd rpo_traversal with
       | Some n -> n
       | None -> failwith "in driver, cfg building init sol, couldn't get first node"
     in
     let last_node = match Seq.hd po_traversal with
       | Some n -> n
       | None -> failwith "in driver, cfg building init sol, couldn't get last node"
     in
     (* let with_args = G.Node.Map.set G.Node.Map.empty ~key:first_node ~data:final_env in *)
     (* let init_sol = Solution.create with_args empty in *)
     let stop = Analysis_profiling.record_stop_time start in
     let () = Analysis_profiling.record_duration_for subname InitEnvSetup stop in
     (* let () = printf "Running abstract interpreter\n%!" in *)

     (* let start = Analysis_profiling.record_start_time () in *)
     (* let analysis_results = Graphlib.fixpoint *)
     (*                          (module G) *)
     (*                          cfg *)
     (*                          (\* ~steps:E.widen_threshold *\) *)
     (*                          ~step:E.widen_with_step *)
     (*                          ~init:init_sol *)
     (*                          ~equal:E.equal *)
     (*                          ~merge:E.merge *)
     (*                          ~f:(fun cc -> *)
     (*                            let tid = Calling_context.to_insn_tid cc in *)
     (*                            let elt = match Tid_map.find tidmap tid with *)
     (*                              | Some elt -> elt *)
     (*                              | None -> *)
     (*                                 failwith @@ *)
     (*                                   sprintf *)
     (*                                     "in calculating analysis_results, couldn't find tid %a in tidmap" *)
     (*                                     Tid.pps tid *)

     (*                            in *)
     (*                            AbsInt.denote_elt subname elt) *)
     (* in *)
     (* let () = printf "Done running abstract interpreter\n%!" in *)
     (* let stop = Analysis_profiling.record_stop_time start in *)
     (* let () = Analysis_profiling.record_duration_for subname AbsInt stop in *)

     let () = printf "Running dependency analysis\n%!" in
     let start = Analysis_profiling.record_start_time () in
     let init_mapping = G.Node.Map.empty in
     let init_sol_dep_analysis = Solution.create
                                   init_mapping
                                   Dependency_analysis.empty
     in
     let dep_analysis_results = Graphlib.fixpoint
                                  (module G)
                                  cfg
                                  ~step:Dependency_analysis.step
                                  ~init:init_sol_dep_analysis
                                  ~equal:Dependency_analysis.equal
                                  ~merge:Dependency_analysis.merge
                                  ~f:(fun cc ->
                                    let tid = Calling_context.to_insn_tid cc in
                                    let elt = match Tid_map.find tidmap tid with
                                  | Some elt -> elt
                                  | None ->
                                     failwith @@
                                       sprintf
                                         "in calculating dep_analysis_results, couldn't find tid %a in tidmap"
                                         Tid.pps tid

                                    in
                                    Dependency_analysis.denote_elt elt)
     in
     let final_dep_analysis_res = Solution.get dep_analysis_results last_node in
     let final_dep_analysis_res = Dependency_analysis.add_flag_ownership_dependencies
                                    flagownership
                                    rpo_traversal
                                    final_dep_analysis_res
     in
     let stop = Analysis_profiling.record_stop_time start in
     let () = Analysis_profiling.record_duration_for
                subname
                NewDependenceAnalysis
                stop
     in
     let () = printf "Done running dependency analysis\n%!" in

     let cond_scrape_st = Trace.ConditionFinder.init
                            ~rpo_traversal
                            ~tidmap
                            ~dep_analysis:final_dep_analysis_res in
     let live_flags =
       Trace.ConditionFinder.FlagScraper.get_live_flags cond_scrape_st in
     let cmov_cnd_flags =
       List.filter live_flags ~f:(fun lf ->
           Trace.ConditionFinder.FlagScraper.flag_used_in_cmov lf cond_scrape_st) in
     let cond_extractor_st = TraceDir.Extractor.init
                               tidmap
                               dep_analysis_results
                               final_dep_analysis_res in
     let dirs = List.fold cmov_cnd_flags ~init:[] ~f:(fun dirs lf ->
                    let split_dir = TraceDir.Extractor.get_conds_for_flag
                                      lf
                                      cond_extractor_st in
                    let combine_dir = TraceDir.Extractor.get_merge_point_for_flag_dirs
                                        cond_extractor_st
                                        lf
                                        split_dir in
                    match combine_dir with
                    | None -> dirs
                    | Some combine_dir ->
                       split_dir :: combine_dir :: dirs) in
     let directive_map = TraceDir.Extractor.to_directive_map dirs in

     let () = printf "Running trace part abstract interpreter\n%!" in

     let start = Analysis_profiling.record_start_time () in

     let init_trace_env = TraceEnv.default_with_env final_env in
     let init_mapping = G.Node.Map.set
                          G.Node.Map.empty
                          ~key:first_node
                          ~data:init_trace_env in
     let init_sol = Solution.create init_mapping TraceEnv.empty in
     let analysis_results = Graphlib.fixpoint
                              (module G)
                              cfg
                              ~step:TraceEnv.widen_with_step
                              ~init:init_sol
                              ~equal:TraceEnv.equal
                              ~merge:TraceEnv.merge
                              ~f:(fun cc ->
                                let tid = Calling_context.to_insn_tid cc in
                                let elt = match Tid_map.find tidmap tid with
                                  | Some elt -> elt
                                  | None ->
                                     failwith @@
                                       sprintf
                                         "in calculating analysis_results, couldn't find tid %a in tidmap"
                                         Tid.pps tid

                                in
                                TraceAbsInt.denote_elt subname directive_map elt) in

     let () = printf "Done running trace part abstract interpreter\n%!" in
     let stop = Analysis_profiling.record_stop_time start in
     let () = Analysis_profiling.record_duration_for subname AbsInt stop in

     let no_symex = Extension.Configuration.get ctxt Common.no_symex_param in
     let use_symex = not no_symex in
     let symex_profiling_out_file = Extension.Configuration.get ctxt Common.symex_profiling_output_file_path_param in

     let debug_fns = ["x86silentstorestest_ADD32mi8_transformed";
                      "x86silentstorestest_ADD32mr_transformed";
                      "x86silentstorestest_ADD64mi8_transformed";
                      "x86silentstorestest_AND32mr_transformed";
                      "x86silentstorestest_MOV8mr_NOREX_transformed"] in
     let () = if List.mem ~equal:String.equal debug_fns subname
              then
                (* print the sub *)
                let () = printf "%a\n%!" Sub.ppo sub in
                let print_sol (cc, env) =
                  let tid = Calling_context.to_insn_tid cc in
                  printf "[Trace] tid %a finished with env:\n%!" Tid.ppo tid;
                  TraceEnv.pp env in
                (* print the trace parted abs int solution *)
                Solution.enum analysis_results
                |> Seq.iter ~f:print_sol
              else () in

     let module TracePartCheckerOracle
                : Common.CheckerInterp with type t := FinalDomain.t =
       struct
         let denote_exp (tid : tid) (exp : Bil.exp) : FinalDomain.t list =
           let cc = Calling_context.of_tid tid in
           let in_state = Solution.get analysis_results cc in
           TraceAbsInt.nondet_denote_exp exp in_state
       end
     in

     let module CompSimpChecker = Comp_simp.Checker(FinalDomain)(TracePartCheckerOracle) in
     let module SSChecker = Silent_stores.Checker(FinalDomain)(TracePartCheckerOracle) in
     
     let combine_res x y = Common.combine_checker_res x y Alert.Set.union in

     let emp = {
         warns = Alert.Set.empty;
         cs_stats = Common.EvalStats.init;
         ss_stats = Common.EvalStats.init
       }
     in

     let elt_of_tid tid = match Tid_map.find tidmap tid with
       | Some elt -> elt
       | None ->
          failwith @@ sprintf "In running checker on sub %s, couldn't find tid %a"
                        subname Tid.pps tid
     in

     let defs = ref None in

     let () = printf "Running checkers\n%!" in
     let start = Analysis_profiling.record_start_time () in
     let all_results = List.fold edges ~init:emp ~f:(fun all_results (_, to_cc, _) ->
                           let to_tid = Calling_context.to_insn_tid to_cc in
                           let elt = elt_of_tid to_tid in
                           let cs_chkr_res = CompSimpChecker.check_elt
                                               subname
                                               to_tid
                                               elt
                           in
                           let ss_chkr_res = SSChecker.check_elt
                                               use_symex
                                               sub
                                               to_tid
                                               idx_st
                                               defs
                                               symex_profiling_out_file
                                               final_dep_analysis_res
                                               tidmap
                                               elt
                           in
                           combine_res all_results @@
                             combine_res ss_chkr_res cs_chkr_res
                         )
     in
     let stop = Analysis_profiling.record_stop_time start in
     let () = Analysis_profiling.record_duration_for subname CsChecking stop in
     let () = printf "Done running checkers\n%!" in

     let all_alerts = all_results.warns in

     let () = printf "Running insn idx filler\n%!" in
     let start = Analysis_profiling.record_start_time () in
     let all_alerts = Alert.InsnIdxFiller.set_for_alert_set idx_st all_alerts in
     let stop = Analysis_profiling.record_stop_time start in
     let () = Analysis_profiling.record_duration_for subname AlertIdxFilling stop in
     let () = printf "Done running insn idx filler\n%!" in
     
     (* this is really dependency analysis info, not liveness info *)
     let () = printf "Running flags live out filler\n%!" in
     let start = Analysis_profiling.record_start_time () in
     (* let all_alerts = Alert.LivenessFiller.set_for_alert_set all_alerts liveness in
      *)
     let all_alerts = Alert.FlagsLiveOutFiller.set_for_alert_set tidmap flagownership final_dep_analysis_res all_alerts in
     let stop = Analysis_profiling.record_stop_time start in
     let () = Analysis_profiling.record_duration_for subname AlertDependencyFilling stop in
     let () = printf "Done running flags live out filler\n%!" in
     
     (* here, liveness means classical dataflow liveness *)
     let () = printf "Running classical dataflow liveness\n%!" in
     let start = Analysis_profiling.record_start_time () in
     let dataflow_liveness = Liveness.run_on_cfg (module G) cfg tidmap liveness in
     let stop = Analysis_profiling.record_stop_time start in
     let () = Analysis_profiling.record_duration_for subname ClassicLiveness stop in
     let () = printf "Done running classical dataflow liveness\n%!" in

     let () = printf "Setting dataflow liveness in alerts\n%!" in
     let start = Analysis_profiling.record_start_time () in
     let all_alerts = Alert.DataflowLivenessFiller.set_for_alert_set all_alerts dataflow_liveness
     in
     let stop = Analysis_profiling.record_stop_time start in
     let () = Analysis_profiling.record_duration_for subname AlertLivenessFilling stop in
     let () = printf "Done setting dataflow liveness in alerts\n%!" in

     let () = printf "Getting callees for analysis\n%!" in
     let start = Analysis_profiling.record_start_time () in
     
     let module GetCallees = Callees.Getter(FinalDomain) in
     let eval_indirect_exp : tid -> Bil.exp -> FinalDomain.t list = fun tid exp ->
       let cc = Calling_context.of_tid tid in
       let env = Solution.get analysis_results cc in
       TraceAbsInt.nondet_denote_exp exp env
     in
     
     let callee_analysis_results = GetCallees.get sub proj eval_indirect_exp in
     let callees = List.filter callee_analysis_results ~f:Or_error.is_ok
                   |> List.map ~f:Or_error.ok_exn
                   |> CalleeRel.Set.of_list
     in
     let stop = Analysis_profiling.record_stop_time start in
     
     let () = Analysis_profiling.record_duration_for subname CalleeAnalysis stop in
     let () = printf "Done getting callees for analysis\n%!" in
     { alerts = all_alerts;
       callees = callees;
       csevalstats = all_results.cs_stats;
       ssevalstats = all_results.ss_stats;
       liveness_info = liveness }

(* this fn needs to have return type unit b/c BAP passes
   should have type (Project.t -> unit). here, this function
   gets curried until it has this type.
 *)
let check_config config img ctxt proj : unit =
  let () = Random.self_init () in
  let target_fns = Config.get_target_fns_exn config proj in
  let worklist = SubSet.of_sequence target_fns in
  let processed = SubSet.empty in
  let init_res = Alert.Set.empty in
  let global_store_data = Global_function_pointers.Libsodium.Analysis.get_all_init_fn_ptr_data ctxt proj in
  let () = Format.printf "Global stores are:\n%!";
           List.iter global_store_data ~f:(fun { data; addr } ->
                       Format.printf "mem[%a] <- %a\n%!"
                                     Word.pp addr
                                     Word.pp data)
  in

  let () = printf "Computing all return insns:\n%!" in
  let () = ReturnInsnsGetter.compute_all_returns () in
  let () = printf "Done.\n%!" in

  let () = printf "Computing flag ownership:\n%!" in
  let flagownership = Flag_ownership.run () in
  let () = printf "Done.\n%!" in

  let do_ss_checks = Extension.Configuration.get ctxt Common.do_ss_checks_param in
  let do_cs_checks = Extension.Configuration.get ctxt Common.do_cs_checks_param in
  let rec loop ~(worklist : SubSet.t)
               ~(processed : SubSet.t)
               ~(res : checker_alerts)
               ~(callees : CRS.t)
               ~(liveness : Live_variables.t)
               ~(is_toplevel : bool)
               ~(csevalstats : EvalStats.t)
               ~(ssevalstats : EvalStats.t)
               ~(config : Config.t) : analysis_result =
    let worklist_wo_procd = Set.diff worklist processed in
    if SubSet.is_empty worklist_wo_procd
    then { alerts = res; csevalstats; ssevalstats; callees } 
    else
      let sub = Set.min_elt_exn worklist_wo_procd in
      let worklist_wo_procd_wo_sub = Set.remove worklist_wo_procd sub in
      let next_processed = Set.add processed sub in
      let () = Format.printf "Processing sub %s (%a)\n%!" (Sub.name sub)
                             Tid.pp (Term.tid sub) in
      if AnalysisBlackList.sub_is_blacklisted sub ||
           AnalysisBlackList.sub_is_not_linked sub
      then
        let () = Format.printf "Sub %s is blacklisted or not linked into the object file, skipping...\n%!"
                   (Sub.name sub) in
        loop ~worklist:worklist_wo_procd_wo_sub
          ~processed:next_processed
          ~res
          ~liveness
          ~callees
          ~csevalstats
          ~ssevalstats
          ~is_toplevel:false
          ~config
      else
        let current_res = run_analyses sub img proj ctxt
                                       ~is_toplevel
                                       ~bss_init_stores:global_store_data
                                       ~do_cs_checks
                                       ~do_ss_checks
                                       ~config
                                       ~flagownership
        in
        let callee_subs = CRS.to_list current_res.callees
                          |> List.map ~f:(fun (r : CR.t) -> sub_of_tid_exn r.callee proj)
                          |> SubSet.of_list
        in
        let () = SubSet.iter callee_subs ~f:(fun callee ->
            printf "CallOf: (%s, %s)\n%!" (Sub.name sub) (Sub.name callee)) in
        let next_worklist = SubSet.union worklist_wo_procd_wo_sub callee_subs in
        let all_alerts = Alert.Set.union res current_res.alerts in
        let total_cs_stats = EvalStats.combine current_res.csevalstats csevalstats in
        let total_ss_stats = EvalStats.combine current_res.ssevalstats ssevalstats in
        loop ~worklist:next_worklist
          ~processed:next_processed
          ~res:all_alerts
          ~liveness:current_res.liveness_info
          ~callees:current_res.callees
          ~config
          ~csevalstats:total_cs_stats
          ~ssevalstats:total_ss_stats
          ~is_toplevel:false
  in
  (* Run the analyses and checkers *)
  let analysis_results = loop ~worklist
                           ~processed
                           ~liveness:Live_variables.IsUsedPass.UseRel.empty
                           ~callees:CRS.empty
                           ~res:init_res
                           ~csevalstats:EvalStats.init
                           ~ssevalstats:EvalStats.init
                           ~is_toplevel:true
                           ~config in
  (* post-processing *)
  let all_alerts = analysis_results.alerts in
  let all_alerts = Alert.OpcodeAndAddrFiller.set_for_alert_set all_alerts proj in
  let all_alerts = Alert.RemoveAllEmptySubName.set_for_alert_set all_alerts proj in
  let all_alerts = Alert.RemoveSpuriousCompSimpAlerts.set_for_alert_set all_alerts proj in
  let all_alerts = Alert.RemoveAlertsForCallInsns.set_for_alert_set all_alerts proj in
  let all_alerts = Alert.RemoveAndWarnEmptyInsnIdxAlerts.set_for_alert_set all_alerts proj in
  let all_alerts, unsupported_count = Alert.RemoveUnsupportedMirOpcodes.set_for_alert_set all_alerts proj in
  let cs_stats = analysis_results.csevalstats in
  let ss_stats = analysis_results.ssevalstats in
  let () = Format.printf "Done processing all functions\n%!" in
  let () = printf "cs stats:\n%s%!" @@ EvalStats.to_json_string cs_stats in
  let () = printf "ss stats:\n%s%!" @@ EvalStats.to_json_string ss_stats in
  let () = printf "\n%!";
           printf "num alerts removed due to unsupported MIR opcodes: %d\n%!"
             unsupported_count in
  let csv_out_file_name = Extension.Configuration.get ctxt Common.output_csv_file_param in
  let () = printf "writing checker alerts to file: %s\n%!" csv_out_file_name in
  Alert.save_alerts_to_csv_file ~filename:csv_out_file_name all_alerts;
  Analysis_profiling.print_all_times ()
