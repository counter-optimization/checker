open Core
open Bap.Std
open Graphlib.Std
open Common
open Monads.Std

open Abstract

module Stats = Common.EvalStats

module Checker(N : Abstract.NumericDomain)
         (Interp : Common.CheckerInterp with type t := N.t) = struct
  type st = {
      tid : tid;
      subname : string;
      alerts : Alert.Set.t;
      eval_stats : Stats.t;
    }

  let emp = Alert.Set.empty

  let dep_bound = 25

  let init_st subname tid = {
      tid;
      subname;
      alerts = emp;
      eval_stats = Stats.init;
    }

  let merge_st st1 st2 = {
      st1 with
      alerts = Alert.Set.union st1.alerts st2.alerts;
      eval_stats = Stats.combine st1.eval_stats st2.eval_stats
    }

  let estats_incr_total_considered st =
    { st with
      eval_stats = Stats.incr_total_considered st.eval_stats }

  let estats_incr_taint_pruned st =
    { st with
      eval_stats = Stats.incr_taint_pruned st.eval_stats }

  let estats_incr_interval_pruned st =
    { st with
      eval_stats = Stats.incr_interval_pruned st.eval_stats }

  let estats_incr_symex_pruned st =
    { st with
      eval_stats = Stats.incr_symex_pruned st.eval_stats }

  let get_intvl : N.t -> Wrapping_interval.t =
      match N.get Wrapping_interval.key with
      | Some f -> f
      | None -> failwith "Couldn't extract interval information out of product domain, in module Silent_stores.Checker"

  let get_taint : N.t -> Checker_taint.Analysis.t =
    match N.get Checker_taint.Analysis.key with
    | Some f -> f
    | None -> failwith "Couldn't extract taint information out of product domain, in module Silent_stores.Checker"

  let is_tainted n =
    match get_taint n with
    | Checker_taint.Analysis.Notaint -> false
    | Checker_taint.Analysis.Taint -> true

  let build_alert ~tid ~desc ~left_val ~right_val ~subname : Alert.t =
    let tid = Some tid in
    let desc = desc in
    let left_val = Some (Wrapping_interval.to_string left_val) in
    let right_val = Some (Wrapping_interval.to_string right_val) in
    let reason = Alert.SilentStores in
    let sub_name = Some subname in
    let problematic_operands = Some [0] in
    let opcode = None in
    let addr = None in
    let rpo_idx = None in
    let flags_live = SS.empty in
    let flags_live_in = SS.empty in
    let is_live = None in
    { tid; desc; left_val; right_val;
      reason; sub_name; problematic_operands;
      opcode; addr; rpo_idx; flags_live_in;
      flags_live; is_live }

  let defs_of_sub sub : def term list =
    Term.enum blk_t sub
    |> Seq.map ~f:(Term.enum def_t)
    |> Seq.join
    |> Seq.to_list

  let history_should_include_insn def idx_st : bool =
    let rhs_exp = Def.rhs def in
    let sym_compilable = Symbolic.Executor.supports_exp rhs_exp in
    sym_compilable &&
      (not @@ Idx_calculator.is_part_of_idx_insn idx_st @@ Term.tid def)

  let get_prev_n_insns n sub idx_st tid : def term list =
    let rec record_n_insns_after
              ?(recorded : def term list = [])
              ?(target_found : bool = false)
              ?(n : int = n)
              idx_st
              target_tid
              all_insns : def term list =
      if Seq.is_empty all_insns || n = 0
      then recorded
      else
        let cur_def = Seq.hd_exn all_insns in
        let all_insns = match Seq.tl all_insns with
          | Some all_insns -> all_insns
          | None -> failwith "Shouldn't happen in get_prev_n_insns"
        in
        if target_found
        then
          let recorded = if history_should_include_insn cur_def idx_st
                         then cur_def :: recorded
                         else recorded in
          record_n_insns_after
            ~recorded
            ~target_found
            ~n:(n - 1)
            idx_st
            target_tid
            all_insns
        else
          let cur_tid = Term.tid cur_def in
          let target_found = Tid.equal target_tid cur_tid in
          let recorded = if target_found then cur_def :: recorded else recorded in
          record_n_insns_after
            ~recorded
            ~target_found
            ~n
            idx_st
            target_tid
            all_insns
    in
    let cfg = Sub.to_cfg sub in
    let nodes = Graphlib.postorder_traverse (module Graphs.Ir) cfg in
    let blks = Seq.map nodes ~f:Graphs.Ir.Node.label in
    let insns = Seq.map blks ~f:(Term.enum def_t) in
    let all_insns : def term Seq.t = Seq.join insns |> Seq.to_list |> List.rev |> Seq.of_list in
    let n_insns_before = record_n_insns_after idx_st tid all_insns in
    n_insns_before

  let check_elt (do_symex : bool) (sub : sub term) 
        (tid : tid) (idx_st : Idx_calculator.t)
        (all_defs_of_sub : def term list option ref)
        (profiling_data_path : string)
        (elt : Blk.elt) : Alert.Set.t Common.checker_res =
    let subname = Sub.name sub in
    let st = init_st subname tid in
    let empty_stats = Common.EvalStats.init in
    let empty_res st = { warns = emp;
                         cs_stats = empty_stats;
                         ss_stats = st.eval_stats }
    in
    match elt with
    | `Def d ->
       let rhs = Def.rhs d in
       begin
         match rhs with
         | Bil.Store (mem, idx, new_data, endian, size) ->
            let st = estats_incr_total_considered st in
            let new_data = Interp.denote_exp st.tid new_data in
            
            let load_of_prev_data = Bil.Load (mem, idx, endian, size) in
            let prev_data = Interp.denote_exp st.tid load_of_prev_data in

            if is_tainted prev_data || is_tainted new_data
            then
              let new_intvl = get_intvl new_data in
              let old_intvl = get_intvl prev_data in
              let interval_could_be_eq =
                Wrapping_interval.could_be_true @@
                  Wrapping_interval.booleq old_intvl new_intvl in
              if interval_could_be_eq
              then
                if do_symex
                then
                  let deps = get_prev_n_insns dep_bound sub idx_st tid in
                  let all_defs_of_sub = if Option.is_none !all_defs_of_sub
                                        then
                                          let defs = defs_of_sub sub in
                                          all_defs_of_sub := Some defs;
                                          defs
                                        else
                                          Option.value_exn !all_defs_of_sub
                  in
                                          
                  let type_info = Type_determination.run
                                    all_defs_of_sub
                                    Common.AMD64SystemVABI.size_of_var_name
                  in
                  let dependent_vars = Var_name_collector.run_on_defs deps in
                  let type_info = Type_determination.narrow_to_vars
                                    dependent_vars
                                    type_info
                  in
                  let do_check = Symbolic.Executor.eval_def_list deps in
                  let init_st = Symbolic.Executor.init
                                  ~do_ss:true
                                  deps
                                  tid
                                  type_info
                                  profiling_data_path
                  in
                  let (), fini_st = Symbolic.Executor.run do_check init_st in
                  if fini_st.failed_ss
                  then
                    empty_res @@ estats_incr_symex_pruned st
                  else
                    let alert = build_alert ~tid ~subname
                                ~left_val:old_intvl
                                ~right_val:new_intvl
                                ~desc:"[SilentStores] failed interval and symex check"
                  in
                  let alerts = Alert.Set.singleton alert in
                  { warns = alerts;
                    cs_stats = empty_stats;
                    ss_stats = st.eval_stats }
                else
                  let alert = build_alert ~tid ~subname
                                ~left_val:old_intvl
                                ~right_val:new_intvl
                                ~desc:"[SilentStores] wrapping interval equality"
                  in
                  let alerts = Alert.Set.singleton alert in
                  { warns = alerts;
                    cs_stats = empty_stats;
                    ss_stats = st.eval_stats }
              else
                empty_res @@ estats_incr_interval_pruned st
            else
              empty_res @@ estats_incr_taint_pruned st
         | _ -> empty_res st
       end
    | _ -> empty_res st
end
