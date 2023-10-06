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
module WithBitvectors = DomainProduct(WithTypes)(Abstract_bitvector)
module FinalDomain : Numeric_domain.Sig = DomainProduct(WithBitvectors)(Bases_domain)

module E = Abstract_memory.Make(FinalDomain)
module R = Region
module Rt = Region.Set
module Vt = struct type t = Common.cell_t end
module AbsInt = AbstractInterpreter(FinalDomain)(R)(Rt)(Vt)(E)

module TraceAbsInt = Trace.AbsInt.Make(FinalDomain)(E)
module TraceDir = Trace.Directives(FinalDomain)(E)
module TraceEnv = Trace.Env(FinalDomain)(E)

let src = Uc_log.create_src "Driver"
let header = Uc_log.create_header src

type analysis_result = {
  callees : CRS.t;
  alerts : Alert.Set.t;
}

type exn += EntryNodeNotFound

let emp_analysis_result = {
  callees = CRS.empty;
  alerts = Alert.Set.empty
}

module SubSet = struct
  include Set.Make_binable_using_comparator(Sub)
end

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
let should_skip_analysis (edges : Edge_builder.edges)
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

let first_insn_cc (g : Graphs.Tid.t) : Calling_context.t =
  let entry = Graphs.Tid.start in
  let firsts = Graphs.Tid.Node.succs entry g in
  let first_node = if Seq.length firsts = 1
    then
      let first_node = Seq.hd_exn firsts in
      Logs.debug ~src (fun m ->
        m ~header "first node is %a" Tid.pp first_node);
      first_node
    else raise EntryNodeNotFound
  in
  Calling_context.of_tid first_node

let do_ (type a) ~(if_ : bool) ~(default : a) (f : unit -> a) : a =
    if if_ then f () else default

let run_analyses sub img proj ~(is_toplevel : bool)
      ~(bss_init_stores : Global_function_pointers.global_const_store list)
      ~(config : Config.t)
      ~(do_ss_checks : bool)
      ~(do_cs_checks : bool)
      ~(flagownership : Flag_ownership.t)
      ctxt : analysis_result =
  let subname = Sub.name sub in
  let subtid = Term.tid sub in
  
  Logs.info ~src (fun m -> m "Analyzing %s" subname);
  Uc_stats.AnalysisInfo.record_analyzed_sub subname subtid;
  
  let should_dump_bir = Extension.Configuration.get ctxt Common.debug_dump in
  do_ ~if_:should_dump_bir ~default:() (fun () ->
    Logs.debug ~src (fun m -> m "%a" Sub.pp sub));
  
  let prog = Project.program proj in
  let tid_graph = Sub.to_graph sub in
  
                                     
  let irg = Sub.to_cfg sub in
  let irg_rpo = Graphlib.reverse_postorder_traverse
                  (module IrCfg)
                  irg
                |> Seq.map ~f:IrCfg.Node.label
  in
  let succ = fun t -> Graphs.Tid.Node.succs t tid_graph in
  (* let idx_st = Idx_calculator.build sub in *)
  let idx_st = Uc_single_shot_pass.run_single
                 (module Idx_calculator.Pass)
                 irg_rpo
  in
  let idx_st = Idx_calculator.Pass.get_state ~succ idx_st in
  let start = Analysis_profiling.record_start_time () in
  let edges, tidmap = Edge_builder.run_one sub proj idx_st in
  let stop = Analysis_profiling.record_stop_time start in
  let () = Analysis_profiling.record_duration_for subname Edgebuilding stop in
  match should_skip_analysis edges tidmap sub prog with
  | Some res ->
    printf "[Driver] Skipping analysis of single jmp subroutine %s\n%!" subname;
    res
  | None ->
    (* CFG *)
    let do_cs = Extension.Configuration.get ctxt Common.do_cs_checks_param in
    let do_ss = Extension.Configuration.get ctxt Common.do_ss_checks_param in
    let do_dmp = Extension.Configuration.get ctxt Common.do_dmp_checks_param in
    
    let start = Analysis_profiling.record_start_time () in
          
    let edges = List.map edges ~f:(Edge_builder.to_cc_edge) in
    let module G = Graphlib.Make(Calling_context)(Bool) in
    let cfg = Graphlib.create (module G) ~edges () in
    
    let stop = Analysis_profiling.record_stop_time start in
    let () = Analysis_profiling.record_duration_for subname CfgCreation stop in

    (* here, liveness means classical dataflow liveness *)
    Logs.info ~src (fun m ->
      m ~header "Running classical dataflow liveness 1");
    let start = Analysis_profiling.record_start_time () in
    let dataflow_liveness = Liveness.run_on_cfg (module G) cfg tidmap in
    let stop = Analysis_profiling.record_stop_time start in
    let () = Analysis_profiling.record_duration_for subname ClassicLivenessOne stop in
    Logs.info ~src (fun m ->
      m ~header "Done running classical dataflow liveness 1");

    let start = Analysis_profiling.record_start_time () in
    let dead_defs = Liveness.get_dead_defs dataflow_liveness tidmap in
    let edges = Edge_builder.remove_dead_defs edges dead_defs in
    let cfg = Graphlib.create (module G) ~edges () in
    let stop = Analysis_profiling.record_stop_time start in
    Analysis_profiling.record_duration_for subname RemoveDeadFlagDefs stop;

    Logs.info ~src (fun m ->
      m ~header "Running classical dataflow liveness 2");
    let start = Analysis_profiling.record_start_time () in
    let dataflow_liveness = Liveness.run_on_cfg (module G) cfg tidmap in
    let stop = Analysis_profiling.record_stop_time start in
    Analysis_profiling.record_duration_for subname ClassicLivenessTwo stop;
    Logs.info ~src (fun m ->
      m ~header "Done running classical dataflow liveness 1");

    (* dmp checker specific *)
    let start = Analysis_profiling.record_start_time () in
    (* let irg_rpo = Graphlib.reverse_postorder_traverse (module IrCfg) irg in *)
    let lahf_sahf = Lahf_and_sahf.run_on_cfg (module G) cfg tidmap in
    let stop = Analysis_profiling.record_stop_time start in
    Analysis_profiling.record_duration_for
      subname
      Analysis_profiling.LahfSahfAnalysis
      stop;

    Lahf_and_sahf.print lahf_sahf;

    do_ ~if_:do_dmp ~default:() (fun () ->
      Dmp_helpers.find_smalloc proj;
      Dmp_helpers.print_smalloc_addrs ());

    let start = Analysis_profiling.record_start_time () in
    let dmp_bt_guards = Dmp_helpers.find_guard_points sub in
    let stop = Analysis_profiling.record_stop_time start in
    Analysis_profiling.record_duration_for
      subname
      Analysis_profiling.DmpGuardPointAnalysis
      stop;
    
    let dmp_bt_set tid (st : TraceEnv.t) : TraceEnv.t =
      let open Abstract_bitvector in
      let tree = match Dmp_helpers.get_guard dmp_bt_guards tid with
        | Some guards ->
          Logs.debug ~src (fun m ->
            m "Found guard at tid %a" Tid.pp tid);
          List.fold !guards ~init:st.tree
            ~f:(fun t {var;set} ->
              TraceEnv.Tree.map t ~f:(fun st ->
                let v = E.lookup var st in
                let bv = of_prod FinalDomain.get v in
                let f = if set then set_60_bit else clear_60_bit in
                let bv = f bv in
                let v = set_in_prod FinalDomain.set v bv in
                Logs.debug ~src
                  (fun m -> m "%a bv: %s\n%!"
                              Tid.pp tid (to_string bv));
                E.set var v st))
        | None -> st.tree
      in
      { tree }
    in

    (* set up initial solution *)
    let start = Analysis_profiling.record_start_time () in
    let () = printf "[Driver] Setting up initial solution \n%!" in
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
      | Error e -> failwith @@ Error.to_string_hum e in
    let env_with_img_set = E.set_img env_with_rsp_set img in
    (* e.g., filter out bap's 'mem' var and the result var
       commonly used in prog analysis *)
    let true_args = List.append argnames freenames
                    |> List.filter ~f:ABI.var_name_is_arg in
    let final_env = List.fold true_args
                      ~init:env_with_img_set
                      ~f:(fun mem argname ->
                        E.init_arg ~name:argname config sub mem) in
    
    let rpo_traversal = Graphlib.reverse_postorder_traverse (module G) cfg in
    let first_node = first_insn_cc tid_graph in

    let stop = Analysis_profiling.record_stop_time start in
    let () = Analysis_profiling.record_duration_for subname InitEnvSetup stop in

    let () = printf "[Driver] running reaching defs\n%!" in
    let start = Analysis_profiling.record_start_time () in
    let reachingdefs = Reachingdefs.run_on_cfg (module G) cfg sub tidmap flagownership first_node in
    let stop = Analysis_profiling.record_stop_time start in
    let () = Analysis_profiling.record_duration_for subname ReachingDefs stop in
    let () = printf "[Driver] done running reaching defs\n%!" in

    let stop = Analysis_profiling.record_stop_time start in
    let () = Analysis_profiling.record_duration_for
               subname
               NewDependenceAnalysis
               stop in
    let () = printf "Done running dependency analysis\n%!" in

    let () = printf "[Driver] running trace part pre-analysis\n%!" in
    let cond_scrape_st = Trace.ConditionFinder.init
                           ~rpo_traversal
                           ~tidmap
                           ~reachingdefs in
    let live_flags = Trace.ConditionFinder.FlagScraper.get_live_flags
                       cond_scrape_st in
    let cmov_cnd_flags = List.filter live_flags ~f:(fun lf ->
      Trace.ConditionFinder.FlagScraper.flag_used_in_cmov lf cond_scrape_st) in
  
    let cond_extractor_st = TraceDir.Extractor.init tidmap reachingdefs in
    let dirs = List.fold cmov_cnd_flags ~init:[] ~f:(fun dirs lf ->
      let split_dir = TraceDir.Extractor.get_conds_for_flag lf cond_extractor_st in
      let combine_dir = TraceDir.Extractor.get_merge_point_for_flag_dirs
                          cond_extractor_st lf split_dir in
      match combine_dir with
      | None ->
        let () = printf "[Drive] live flag <%a, %s> couldn't get both split and combine directives\n%!" Tid.ppo (fst lf) (snd lf) in
        dirs
      | Some combine_dir ->
        split_dir :: combine_dir :: dirs) in
    let () = printf "[Driver] trace part dirs are:\n%!";
      List.iter dirs ~f:(fun td ->
        printf "\t%s\n%!" @@ TraceDir.to_string td) in
    let dirs = [] in
    let directive_map = TraceDir.Extractor.to_directive_map dirs in
    let () = printf "[Driver] Done running trace part pre-analysis\n%!" in

    let () = printf "[Driver] Running trace part abstract interpreter\n%!" in

    let start = Analysis_profiling.record_start_time () in

    let init_trace_env = TraceEnv.default_with_env final_env in

    let init_mapping = G.Node.Map.set
                         G.Node.Map.empty
                         ~key:first_node
                         ~data:init_trace_env in
    let init_sol = Solution.create init_mapping TraceEnv.empty in

    (* let init_mapping = IrCfg.Node.Map.set *)
    (*                      IrCfg.Node.Map.empty *)
    (*                      ~key:irg_first_blk *)
    (*                      ~data:init_trace_env in *)
    (* let init_sol = Solution.create init_mapping TraceEnv.empty in *)

    (* let tid_level_data : ((tid, TraceEnv.t, _) Map.t) ref = ref (Map.empty (module Tid)) in *)
    
    let analysis_results = Graphlib.fixpoint
                             (* (module IrCfg) *)
                             (* irg *)
                             (module G)
                             cfg
                             ~step:TraceEnv.widen_with_step
                             ~init:init_sol
                             ~equal:TraceEnv.equal
                             ~merge:TraceEnv.merge
                             (* ~f:(fun node inenv -> *)
                             (*   let blk = IrCfg.Node.label node in *)
                             (*   printf "[Fixpoint] at blk %a, inenv:\n%!" Tid.ppo (Term.tid blk); *)
                             (*   TraceEnv.pp inenv; *)
                             (*   printf "\n%!"; *)
                             (*   let elts = Blk.elts blk in *)
                             (*   let outenv = Seq.fold elts ~init:inenv ~f:(fun inenv elt -> *)
                             (*     let elt_tid = elt_to_tid elt in *)
                             (*     if Idx_calculator.is_part_of_idx_insn idx_st elt_tid *)
                             (*     then inenv *)
                             (*     else match elt with *)
                             (*       | `Def d -> *)
                             (*         printf "[Debug] interpreting tid %a\n%!" Tid.ppo elt_tid; *)
                             (*         tid_level_data := Map.set !tid_level_data ~key:elt_tid ~data:inenv; *)
                             (*         TraceAbsInt.denote_elt subname directive_map elt inenv *)
                             (*       | _ -> inenv) in *)
                             (*   let in_envs = TraceEnv.Tree.map_list Fn.id inenv.tree in *)
                             (*   let out_envs = TraceEnv.Tree.map_list Fn.id outenv.tree in *)
                             (*   printf "[Debug] # in_envs: %d, # out_envs: %d\n%!" (List.length in_envs) (List.length out_envs); *)
                             (*   (match List.zip in_envs out_envs with *)
                             (*    | Ok zipped -> *)
                             (*      let env_diffs = List.map zipped ~f:(fun (i, o) -> *)
                             (*        printf "[Debug] abs mem equal?: %B\n%!" (E.equal i o); *)
                             (*        E.differs i o) in *)
                             (*      printf "[Debug] env_diffs: %s\n%!" (List.to_string env_diffs ~f:(List.to_string ~f:Fn.id)) *)
                             (*    | Unequal_lengths -> printf "[Debug] unequal lengths\n%!"); *)
                             (*   outenv) in *)
                             ~f:(fun cc st ->
                               let tid = Calling_context.to_insn_tid cc in
                               let elt = match Tid_map.find tidmap tid with
                                 | Some elt -> elt
                                 | None ->
                                   failwith @@
                                   sprintf
                                     "in calculating analysis_results, couldn't find tid %a in tidmap"
                                     Tid.pps tid in
                               let st = dmp_bt_set tid st in
                               Logs.debug ~src (fun m -> m "denoting elt %a" Tid.pp tid);
                               TraceAbsInt.denote_elt subname directive_map elt st
                               (* fun st -> *)
                               (*   printf "[Driver] denoting elt %a\n%!" Tid.ppo tid; *)
                               (*   printf "[Driver] instate:\n%!"; *)
                               (*   TraceEnv.pp st; *)
                               (*   TraceAbsInt.denote_elt subname directive_map elt st *)) in
    
    (* let analysis_results = Myfixpoint.compute *)
    (*                          (module IrCfg) *)
    (*                          irg *)
    (*                          ~step:TraceEnv.widen_with_step *)
    (*                          ~init:init_sol *)
    (*                          ~equal:TraceEnv.equal *)
    (*                          ~merge:TraceEnv.merge *)
    (*                          ~f:(fun blk inenv inmap -> *)
    (*                            let blk = IrCfg.Node.label blk in *)
    (*                            let elts = Term.enum def_t blk in *)
    (*                            let (final_env, final_map) = *)
    (*                              Seq.fold elts *)
    (*                                ~init:(inenv, inmap) *)
    (*                                ~f:(fun (inenv, inmap) elt -> *)
    (*                                  let outenv = TraceAbsInt.denote_elt subname directive_map elt inenv in *)
    (*                                  let elt_tid = elt_to_tid elt in *)
    (*                                  (outenv, Map.set inmap ~key:elt_tid ~data:inenv))) in  *)

    let () = printf "[Driver] Done running trace part abstract interpreter\n%!" in
    let stop = Analysis_profiling.record_stop_time start in
    let () = Analysis_profiling.record_duration_for subname AbsInt stop in

    let no_symex = Extension.Configuration.get ctxt Common.no_symex_param in
    let use_symex = not no_symex in
    let symex_profiling_out_file = Extension.Configuration.get ctxt Common.symex_profiling_output_file_path_param in

    let module TracePartCheckerOracle
      : Common.CheckerInterp with type t := FinalDomain.t =
    struct
      let denote_exp (tid : tid) (exp : Bil.exp) : FinalDomain.t list =
        let cc = Calling_context.of_tid tid in
        let in_state = Solution.get analysis_results cc in
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

    Logs.info ~src (fun m -> m "Running checkers");
    let start = Analysis_profiling.record_start_time () in
    let all_alerts = List.fold edges ~init:emp ~f:(fun alerts (_, to_cc, _) ->
      let to_tid = Calling_context.to_insn_tid to_cc in
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
    let stop = Analysis_profiling.record_stop_time start in
    Analysis_profiling.record_duration_for subname CsChecking stop;
    Logs.info ~src (fun m -> m "Done running checkers");
    
    Logs.info ~src (fun m-> m "Running insn idx filler");
    let start = Analysis_profiling.record_start_time () in
    let all_alerts = Alert.InsnIdxFiller.set_for_alert_set idx_st all_alerts in
    let stop = Analysis_profiling.record_stop_time start in
    let () = Analysis_profiling.record_duration_for subname AlertIdxFilling stop in
    let () = printf "Done running insn idx filler\n%!" in

    (* this is really dependency analysis info, not liveness info *)
    let () = printf "Running flags live out filler\n%!" in
    let start = Analysis_profiling.record_start_time () in
    let all_alerts = Alert.FlagsLiveOutFiller.set_for_alert_set tidmap flagownership reachingdefs all_alerts in
    let stop = Analysis_profiling.record_stop_time start in
    let () = Analysis_profiling.record_duration_for subname AlertDependencyFilling stop in
    let () = printf "Done running flags live out filler\n%!" in

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
      (* let env = Map.find_exn !tid_level_data tid in  *)
      TraceAbsInt.nondet_denote_exp exp env
    in

    let callee_analysis_results = GetCallees.get sub proj eval_indirect_exp in
    let callees = List.filter callee_analysis_results ~f:Or_error.is_ok
                  |> List.map ~f:Or_error.ok_exn
                  |> CalleeRel.Set.of_list
    in
    let stop = Analysis_profiling.record_stop_time start in

    let () = Analysis_profiling.record_duration_for subname CalleeAnalysis stop in
    Logs.info ~src (fun m -> m "Callee analysis finished.");
    { alerts = all_alerts;
      callees = callees; }
    
(* this fn needs to have return type unit b/c BAP passes
   should have type (Project.t -> unit). here, this function
   gets curried until it has this type.
*)
let check_config config img ctxt proj : unit =
  Random.self_init ();
  let target_fns = Config.get_target_fns_exn config proj in
  let worklist = SubSet.of_list @@ Sequence.to_list target_fns in
  let processed = SubSet.empty in
  let init_res = Alert.Set.empty in
  
  let global_store_data = Global_function_pointers.Libsodium.Analysis.get_all_init_fn_ptr_data ctxt proj in
  Logs.debug ~src (fun m -> m "Global stores are:");
  List.iter global_store_data ~f:(fun { data; addr } ->
    Logs.debug ~src (fun m -> m "mem[%a] <- %a"
                                Word.pp addr
                                Word.pp data));

  let () = printf "Computing flag ownership:\n%!" in
  let flagownership = Flag_ownership.run () in
  let () = printf "Done.\n%!" in

  let do_ss_checks = Extension.Configuration.get ctxt Common.do_ss_checks_param in
  let do_cs_checks = Extension.Configuration.get ctxt Common.do_cs_checks_param in

  let should_dump_kb = Extension.Configuration.get ctxt Common.debug_dump in
  do_ ~if_:(should_dump_kb) ~default:() (fun () ->
    Format.printf "%a\n%!" KB.pp_state @@ Toplevel.current ());

  let rec loop ~(worklist : SubSet.t)
            ~(processed : SubSet.t)
            ~(res : Alert.Set.t)
            ~(callees : CRS.t)
            ~(is_toplevel : bool)
            ~(config : Config.t) : analysis_result =
    let worklist_wo_procd = Set.diff worklist processed in
    if SubSet.is_empty worklist_wo_procd
    then { alerts = res; callees } 
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
          ~callees
          ~is_toplevel:false
          ~config
      else
        let current_res = run_analyses sub img proj ctxt
                            ~is_toplevel
                            ~bss_init_stores:global_store_data
                            ~do_cs_checks
                            ~do_ss_checks
                            ~config
                            ~flagownership in
        let callee_subs = CRS.to_list current_res.callees
                          |> List.map ~f:(fun (r : CR.t) -> sub_of_tid_exn r.callee proj)
                          |> SubSet.of_list in
        let () = SubSet.iter callee_subs ~f:(fun callee ->
          printf "CallOf: (%s, %s)\n%!" (Sub.name sub) (Sub.name callee)) in
        let next_worklist = SubSet.union worklist_wo_procd_wo_sub callee_subs in
        let all_alerts = Alert.Set.union res current_res.alerts in
        loop ~worklist:next_worklist
          ~processed:next_processed
          ~res:all_alerts
          ~callees:current_res.callees
          ~config
          ~is_toplevel:false
  in
  (* Run the analyses and checkers *)
  let analysis_results = loop ~worklist
                           ~processed
                           ~callees:CRS.empty
                           ~res:init_res
                           ~is_toplevel:true
                           ~config in
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
  
  Logs.info ~src (fun m -> m "Done processing all functions");
  
  Uc_stats.Eval.(info_print cs_stats "cs stats");
  Uc_stats.Eval.(info_print ss_stats "ss stats");
  Uc_stats.Eval.(info_print dmp_stats "dmp stats");

  let unsupported_count = res.num_removed in
  Logs.info ~src (fun m ->
    m "num alerts removed due to unsupported MIR opcodes: %d"
      unsupported_count);
  
  let csv_out_file_name = Extension.Configuration.get ctxt Common.output_csv_file_param in
  Logs.info ~src (fun m ->
    m "writing checker alerts to file: %s" csv_out_file_name);
  Alert.save_alerts_to_csv_file ~filename:csv_out_file_name all_alerts;
  Analysis_profiling.print_all_times ()
