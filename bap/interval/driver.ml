open Core
open Bap_main
open Bap.Std
open Graphlib.Std
module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

open Common
open Abstract_memory

module Cfg = Bap.Std.Graphs.Cfg
module IrCfg = Bap.Std.Graphs.Ir
module CR = Common.CalleeRel
module CRS = CR.Set
module ABI = Common.AMD64SystemVABI

type check_sub_result = { callees : CRS.t;
                          liveness_info : Live_variables.t;
                          alerts : Alert.Set.t }

type checker_alerts = Alert.Set.t

module SubSet = struct
  include Set.Make_binable_using_comparator(Sub)
end

let print_iml iml : unit =
  Format.printf
        "%a\n%!"
        Sexp.pp
        (Map_lattice.Interval.M.sexp_of_t Wrapping_interval.sexp_of_t iml)

let print_sol sol : unit =
  Solution.enum sol |>
    Sequence.iter ~f:(fun (n, iml) ->
        Format.printf "Node (%a): \n%!" Graphs.Ir.Node.pp n;
        print_iml iml)

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
                   liveness_info = Live_variables.IsUsedPass.UseRel.empty }
       end
       | _ -> failwith "in should_skip_analysis, subroutine is single instruction but non jump instruction. this case is not yet handled." 
  else
    None

let run_analyses sub img proj ~(is_toplevel : bool)
                 ~(bss_init_stores : Global_function_pointers.global_const_store list)
                 ~(config : Config.t)
                 ~(do_ss_checks : bool)
                 ~(do_cs_checks : bool) : check_sub_result =
  let () = printf "Running analysis on sub %s\n%!" (Sub.name sub) in
  let prog = Project.program proj in
  let edges, tidmap = Edge_builder.run_one sub proj in
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
     let liveness = Live_variables.Analysis.run sub in
     (* let () = Live_variables.IsUsedPass.print_rels liveness in *)
     
     (* CFG *)
     let edges = List.map edges ~f:(Calling_context.of_edge) in
     let module G = Graphlib.Make(Calling_context)(Bool) in
     let cfg = Graphlib.create (module G) ~edges () in

     (* AbsInt *)
     let module ProdIntvlxTaint = DomainProduct(Wrapping_interval)(Checker_taint.Analysis) in
     let module WithTypes = DomainProduct(ProdIntvlxTaint)(Type_domain) in
     let module FinalDomain = DomainProduct(WithTypes)(Bases_domain) in
     
     let module E = Abstract_memory.Make(FinalDomain) in
     let module R = Region in
     let module Rt = Region.Set in
     let module Vt = struct type t = Common.cell_t end in
     let module AbsInt = AbstractInterpreter(FinalDomain)(R)(Rt)(Vt)(E) in

     (* set up initial solution *)
     let () = printf "Setting up initial solution \n%!" in
     let empty = E.empty in
     let stack_addr = 0x7fff_fff0 in
     
     let free_vars = Sub.free_vars sub in
     let freenames = Set.to_list free_vars |> List.map ~f:Var.name in
     
     let args = Term.enum arg_t sub in
     let argnames = Seq.map args ~f:(fun a -> Arg.var a |> T.Var.name) |> Seq.to_list in
     let () = printf "Arg names of sub (%s) are:\n%!" @@ Sub.name sub;
              List.iter argnames ~f:(fun a -> printf "%s\n%!" a) in

     

     let with_canary_set = E.set_stack_canary empty in

     let env_with_df_set = E.set "DF" FinalDomain.b0 with_canary_set in
     let env_with_rsp_set = match E.set_rsp stack_addr env_with_df_set with
         | Ok env' -> env'
         | Error e -> failwith @@ Error.to_string_hum e in
     let env_with_img_set = E.set_img env_with_rsp_set img in
     let env_with_bss_initd =
       List.fold bss_init_stores
                 ~init:(Ok env_with_img_set)
                 ~f:(fun env' store ->
                   Or_error.bind env' ~f:(fun env' ->
                                   E.store_global env' ~addr:store.addr
                                                  ~data:store.data
                                                  ~valtype:CellType.Ptr)) in
     
     let env_with_bss_initd = match env_with_bss_initd with
       | Ok env -> env 
       | Error e ->
          let inner_err_msg = Error.to_string_hum e in
          failwith @@ sprintf "in run_analysis setting bss inits : %s" inner_err_msg in

     (* let final_env = env_with_bss_initd in *)

     (* e.g., filter out bap's 'mem' var, the result var
        commonly used in prog analysis *)
     let true_args = List.append argnames freenames
                     |> List.filter ~f:ABI.var_name_is_arg in
     
     let final_env = List.fold true_args
                               ~init:env_with_bss_initd
                               ~f:(fun mem argname ->
                                 E.init_arg ~name:argname config sub mem) in
     
     (* let () = Format.printf "Initial memory+env is: %!" in *)
     (* let () = E.pp final_env in *)
     (* let () = Format.printf "\n%!" in *)

     let rpo_traversal = Graphlib.reverse_postorder_traverse (module G) cfg in
     let first_node = match Seq.hd rpo_traversal with
       | Some n -> n
       | None -> failwith "in driver, cfg building init sol, couldn't get first node" in

     let with_args = G.Node.Map.set G.Node.Map.empty ~key:first_node ~data:final_env in
     let init_sol = Solution.create with_args empty in

     let () = printf "Running abstract interpreter\n%!" in
     
     let analysis_results = Graphlib.fixpoint
                              (module G)
                              cfg
                              (* ~steps:E.widen_threshold *)
                              ~step:E.widen_with_step
                              ~init:init_sol
                              ~equal:E.equal
                              ~merge:E.merge
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
                                AbsInt.denote_elt elt) in

     let () = printf "Done running abstract interpreter\n%!" in
     
     (* Build up checker infra and run the checkers
      * This next part is an abomination of Ocaml code
      * todo: refactor this using this SO answer:
      * https://stackoverflow.com/questions/67823455/having-a-module-and-an-instance-of-it-as-parameters-to-an-ocaml-function
      *)
     (* the env to run the checker in is stored in the insn to be checked
          here, the solution envs are stored/keyed by calling context,
          the instructions are still just by tid though. *)
     let analyze_edge (module Chkr : Checker.S with type env = E.t) (e : 'a Calling_context.Edge.t) : Chkr.warns =
       let from_cc = Calling_context.Edge.from_ e in
       let to_cc = Calling_context.Edge.to_ e in
       let to_tid = Calling_context.to_insn_tid to_cc in
       let in_state = Solution.get analysis_results to_cc in
       if E.equal in_state E.empty
       then
         failwith @@
           sprintf "in analyzing edge (%s, %s), with checker %s, elt tid to examine %a, in state env is empty."
                   (Calling_context.to_string from_cc)
                   (Calling_context.to_string to_cc)
                   Chkr.name
                   Tid.pps to_tid
       else
         let insn = match Tid_map.find tidmap to_tid with
           | Some elt -> elt
           | None ->
              failwith @@
                sprintf
                  "In running checker %s, couldn't find tid %a" Chkr.name Tid.pps to_tid in
         (* let () = Format.printf "checking edge (%a, %a)\n%!" *)
         (*                        Calling_context.pp from_cc *)
         (*                        Calling_context.pp to_cc in *)
         Chkr.check_elt insn liveness in_state sub proj in

     let run_checker (module Chkr : Checker.S with type env = E.t) (es : 'a Calling_context.edges) : Chkr.warns =
       List.fold edges
         ~init:Alert.Set.empty
         ~f:(fun alerts edge ->
           let alerts' = analyze_edge (module Chkr) edge in
           Alert.Set.union alerts alerts') in

     let comp_simp_alerts =
       if do_cs_checks
       then
         begin
           let module CSChecker : Checker.S
                      with type env = E.t = struct
               include Comp_simp.Checker(FinalDomain)
               type env = E.t
             end in
           let () = printf "Starting comp simp checker...\n%!" in
           let alerts = run_checker (module CSChecker) edges in
           let () = printf "Done running comp simp checker.\n%!" in
           alerts
         end
       else Alert.Set.empty in

     let silent_store_alerts =
       if do_ss_checks
       then
         begin
           let module SSChecker : Checker.S with type env = E.t = struct
               include Silent_stores.Checker(FinalDomain)
               type env = E.t
             end in
           
           let () = printf "Starting silent stores checker...\n%!" in
           let alerts = run_checker (module SSChecker) edges in
           let () = printf "Done running silent stores checker.\n%!" in
           alerts
         end
       else Alert.Set.empty in
     
     let all_alerts = Alert.Set.union comp_simp_alerts silent_store_alerts in

     let alerts_with_subs = Alert.Set.map all_alerts ~f:(fun alert ->
                                            { alert with sub_name = Some (Sub.name sub) }) in
     let alerts_with_ops_addrs = Alert.OpcodeAndAddrFiller.set_opcode_and_addr_for_alert_set alerts_with_subs proj in
     
     let alerts_with_liveness = Alert.LivenessFiller.set_for_alert_set alerts_with_ops_addrs liveness in

     let alerts_with_rpo_indices = Alert.RpoIdxAlertFiller.set_rpo_indices_for_alert_set alerts_with_liveness sub proj rpo_traversal in
       
     let all_alerts = alerts_with_rpo_indices in
     
     (* get callees--both direct and indirect calls--of this call *)
     let () = Format.printf "Getting callees for sub %s\n%!" (Sub.name sub) in
     
     let module GetCallees = Callees.Getter(FinalDomain) in

     let callee_analysis_results = GetCallees.get sub proj analysis_results in
     let callees = List.filter callee_analysis_results ~f:Or_error.is_ok
                   |> List.map ~f:Or_error.ok_exn
                   |> CalleeRel.Set.of_list in

     (* print all callee errs *)
     let () = List.filter callee_analysis_results ~f:Or_error.is_error
              |> List.iter
                   ~f:(function
                       | Error err ->
                          Format.printf "In getting callees for sub %s : %s\n%!"
                                        (Sub.name sub)
                                        (Error.to_string_hum err)
                       | Ok _ -> ()) in
     
     (* let () = Format.printf "Callees are: \n%!" in *)
     (* let () = CRS.print callees in *)
     
     { alerts = all_alerts; callees = callees; liveness_info = liveness }

let iter_insns sub : unit =
  let irg = Sub.to_cfg sub in
  let free_vars = Sub.free_vars sub in
  let () = Var.Set.iter free_vars ~f:(fun v -> Format.printf "Free var: %s\n%!" (Var.name v)) in
  
  let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) irg in
  
  let print_sub_defs graphnode =
    let bb = Graphs.Ir.Node.label graphnode in
    let insns = Blk.elts bb in
    let print_insn = function
      | `Def d -> Format.printf "iter_insns--Def: %s\n%!" @@ Def.to_string d
      | `Phi p -> Format.printf "iter_insns--Phi: %s\n%!" @@ Phi.to_string p
      | `Jmp j -> Format.printf "iter_insns--Jmp: %s\n%!" @@ Jmp.to_string j in
    Seq.iter insns ~f:print_insn in
  
  let () = Format.printf "nodes are:\n%!" in 
  Seq.iter nodes ~f:print_sub_defs

(* this fn needs to have return type unit b/c BAP passes
   should have type (Project.t -> unit). here, this function
   gets curried until it has this type.
 *)
let check_config config img ctxt proj : unit =
  let target_fns = Config.get_target_fns_exn config proj in
  let worklist = SubSet.of_sequence target_fns in
  let processed = SubSet.empty in
  let init_res = Alert.Set.empty in

  let global_store_data = Global_function_pointers.Libsodium.Analysis.get_all_init_fn_ptr_data ctxt proj in
  let () = Format.printf "Global stores are:\n%!";
           List.iter global_store_data ~f:(fun { data; addr } ->
                       Format.printf "mem[%a] <- %a\n%!"
                                     Word.pp addr
                                     Word.pp data) in

  let do_ss_checks = Extension.Configuration.get ctxt Common.do_ss_checks_param in
  let do_cs_checks = Extension.Configuration.get ctxt Common.do_cs_checks_param in
  
  let rec loop ~(worklist : SubSet.t)
               ~(processed : SubSet.t)
               ~(res : checker_alerts)
               ~(liveness : Live_variables.t)
               ~(is_toplevel : bool)
               ~(config : Config.t) : checker_alerts =
    let worklist_wo_procd = Set.diff worklist processed in
    if SubSet.is_empty worklist_wo_procd
    then res
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
          ~is_toplevel:false
          ~config
      else
        let current_res = run_analyses sub img proj
                                       ~is_toplevel
                                       ~bss_init_stores:global_store_data
                                       ~do_cs_checks
                                       ~do_ss_checks
                                       ~config in
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
          ~liveness:current_res.liveness_info
          ~config
          ~is_toplevel:false in
  
  (* Run the analyses and checkers *)
  let checker_alerts = loop ~worklist
                            ~processed
                            ~liveness:Live_variables.IsUsedPass.UseRel.empty
                            ~res:init_res
                            ~is_toplevel:true
                            ~config in
  let () = Format.printf "Done processing all functions\n%!" in
  
  (* just print to stdout for now *)
  let csv_out_file_name = Extension.Configuration.get ctxt Common.output_csv_file_param in
  Alert.save_alerts_to_csv_file ~filename:csv_out_file_name checker_alerts
