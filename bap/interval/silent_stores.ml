open Core_kernel
open Bap.Std
open Graphlib.Std
open Common
open Monads.Std

open Abstract

module Stats = Common.EvalStats

module DefTermSet = struct
  include Set.Make_binable_using_comparator(Def)
end

module Checker(N : Abstract.NumericDomain)
         (Interp : Common.CheckerInterp with type t := N.t) = struct
  type st = {
      tid : tid;
      subname : string;
      alerts : Alert.Set.t;
      eval_stats : Stats.t;
    }

  let emp = Alert.Set.empty

  let dep_bound = 40

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

  let rec get_prev_defterm ~(of_ : tid) ~(defs : def term list) : def term option =
    match defs with
    | [] -> None
    | x :: [] -> None
    | x :: y :: rst ->
       let ytid = Term.tid y in
       if Tid.equal of_ ytid
       then Some x
       else
         let defs = y :: rst in
         get_prev_defterm ~of_ ~defs

  let store_depends_on ~(prev : def term) ~(target : def term) : bool =
    let prev = Def.rhs prev in
    let target = Def.rhs target in
    match prev, target with
    | Bil.Store (_, p_offs, _, _, _),
      Bil.Store (_, t_offs, _, _, _) -> Exp.equal p_offs t_offs
    | _ -> false

  let get_up_to_n_dependent_insns ~(prev_store : def term option)
        ~(n: int) ~(sub: sub term) ~(for_ : tid)
        ~(deps : Dependency_analysis.t) ~(tidmap : Blk.elt Tid_map.t)
      : def term list =
    let emp = DefTermSet.empty in
    let dt_compare left right =
      let lefttid = Term.tid left in
      let righttid = Term.tid right in
      Tid.compare lefttid righttid in
    let dt_only_lookup ~(tidmap : Blk.elt Tid_map.t) ~(tid: tid) : def term option =
      match Tid_map.find tidmap tid with
      | None -> None
      | Some elt -> (match elt with
                     | `Def d -> Some d
                     | _ -> None) in
    let rec take_n ~(n : int) ~(to_ : DefTermSet.t) from_ =
      if n = 0
      then to_
      else
        match from_ with
        | [] -> to_
        | dt :: from_' -> if DefTermSet.mem to_ dt
                          then take_n ~n ~to_ from_'
                          else
                            let to_ = DefTermSet.add to_ dt in
                            let from_ = from_' in
                            let n = n - 1 in
                            take_n ~n ~to_ from_ in
    let deps_of_dt (dt : def term) =
      let tid = Term.tid dt in
      let uses = Tid_map.find deps.tid_uses tid
                 |> Option.value ~default:Tidset.empty in
      Tidset.fold uses ~init:emp ~f:(fun dts use_tid ->
          match dt_only_lookup ~tidmap ~tid:use_tid with
          | None -> dts
          | Some defterm -> DefTermSet.add dts defterm) in
    let rec loop ~(old: DefTermSet.t)
              ~(added: DefTermSet.t) : DefTermSet.t =
      let newly_added = DefTermSet.diff added old in
      let doesnt_need_processing = DefTermSet.is_empty newly_added in
      if doesnt_need_processing
      then old
      else
        let old_sz = DefTermSet.length old in
        let added_sz = DefTermSet.length newly_added in
        if old_sz + added_sz >= n
        then
          let top_off_elts = DefTermSet.to_list newly_added
                             |> take_n ~n ~to_:old in
          DefTermSet.union old top_off_elts
        else
          let old' = DefTermSet.union old newly_added in
          let added' = DefTermSet.fold newly_added
                         ~init:emp
                         ~f:(fun to_add dt ->
                           Set.union to_add @@ deps_of_dt dt) in
          loop ~old:old' ~added:added' in
    let start_term = Option.value_exn (dt_only_lookup ~tidmap ~tid:for_) in
    let start = DefTermSet.singleton start_term in
    let start = match prev_store with
      | Some prev_store -> DefTermSet.add start prev_store
      | None -> start in
    loop ~old:emp ~added:start
    |> DefTermSet.to_list
    |> List.sort ~compare:dt_compare

  let check_elt (do_symex : bool)
        (sub : sub term) 
        (tid : tid)
        (idx_st : Idx_calculator.t)
        (all_defs_of_sub : def term list option ref)
        (profiling_data_path : string)
        (deps : Dependency_analysis.t)
        (tidmap : Blk.elt Tid_map.t)
        (elt : Blk.elt) : Alert.Set.t Common.checker_res =
    let subname = Sub.name sub in
    let st = init_st subname tid in
    let empty_stats = Common.EvalStats.init in
    let empty_res st = { warns = emp;
                         cs_stats = empty_stats;
                         ss_stats = st.eval_stats } in
    let could_be_eq old new_ =
      let new_intvl = get_intvl new_ in
      let old_intvl = get_intvl old in
      Wrapping_interval.could_be_true @@
        Wrapping_interval.booleq old_intvl new_intvl in
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

            List.cartesian_product new_data prev_data
            |> List.fold ~init:(empty_res st) ~f:(fun res (prev_data, new_data) ->
                   if is_tainted prev_data || is_tainted new_data
                   then
                     if could_be_eq prev_data new_data
                     then
                       if do_symex
                       then
                         let all_defs_of_sub =
                           if Option.is_none !all_defs_of_sub
                           then let defs = defs_of_sub sub in
                                all_defs_of_sub := Some defs;
                                defs
                           else Option.value_exn !all_defs_of_sub in
                         let prev_def_term = get_prev_defterm
                                               ~of_:tid
                                               ~defs:all_defs_of_sub in
                         let prev_store = match prev_def_term with
                           | None -> None
                           | Some prev -> if store_depends_on ~target:d ~prev
                                          then Some prev
                                          else None in
                         let deps = get_up_to_n_dependent_insns
                                      ~prev_store
                                      ~n:dep_bound
                                      ~sub
                                      ~for_:tid
                                      ~deps
                                      ~tidmap in
                         let type_info = Type_determination.run
                                           all_defs_of_sub
                                           Common.AMD64SystemVABI.size_of_var_name in
                         let dependent_vars = Var_name_collector.run_on_defs deps in
                         let type_info = Type_determination.narrow_to_vars
                                           dependent_vars
                                           type_info in
                         let do_check = Symbolic.Executor.eval_def_list deps in
                         let init_st = Symbolic.Executor.init
                                         ~do_ss:true
                                         deps
                                         tid
                                         type_info
                                         profiling_data_path in
                         let (), fini_st = Symbolic.Executor.run do_check init_st in
                         if fini_st.failed_ss
                         then
                           let left_val = get_intvl prev_data in
                           let right_val = get_intvl new_data in
                           let desc = "failed symex check" in
                           let alert = build_alert ~tid ~subname ~left_val ~right_val ~desc in
                           let alerts = Alert.Set.singleton alert in
                           { warns = alerts;
                             cs_stats = empty_stats;
                             ss_stats = st.eval_stats }
                         else empty_res @@ estats_incr_symex_pruned st
                       else
                         let left_val = get_intvl prev_data in
                         let right_val = get_intvl new_data in
                         let desc = "failed interval equality" in
                         let alert = build_alert ~tid ~subname ~left_val ~right_val ~desc in
                         let alerts = Alert.Set.singleton alert in
                         { warns = alerts;
                           cs_stats = empty_stats;
                           ss_stats = st.eval_stats }
                     else empty_res @@ estats_incr_interval_pruned st
                   else empty_res @@ estats_incr_taint_pruned st)
         | _ -> empty_res st
       end
    | _ -> empty_res st
end
