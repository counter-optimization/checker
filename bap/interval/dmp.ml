open Core_kernel
open Bap.Std
open Graphlib.Std
open Bap_primus.Std
open Monads.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB
module ABI = Common.AMD64SystemVABI
module SS = Common.SS
module Stats = Common.EvalStats

module Checker(N : Abstract.NumericDomain)
         (Interp : Common.CheckerInterp with type t := N.t) = struct
  
  let get_taint : N.t -> Checker_taint.Analysis.t =
    match N.get Checker_taint.Analysis.key with
    | Some f -> f
    | None -> failwith "Couldn't extract taint information out of product domain, in module Dmp.Checker"

  let is_tainted n =
    match get_taint n with
    | Checker_taint.Analysis.Notaint -> false
    | Checker_taint.Analysis.Taint -> true

  let build_alert ~tid ~desc ~subname : Alert.t =
    let tid = Some tid in
    let desc = desc in
    let left_val = Some "" in
    let right_val = Some "" in
    let reason = Alert.Dmp in
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
  
  let check_elt sub tid elt : Alert.Set.t Common.checker_res =
    let empty_stats = Common.EvalStats.init in
    let empty_res : Alert.Set.t Common.checker_res = {
        warns = Alert.Set.empty;
        cs_stats = empty_stats;
        ss_stats = empty_stats
      } in
    match elt with
    | `Def d ->
       (match Def.rhs d with
        | Bil.Store (_mem, _idx, store_data, _endian, _size) ->
           let store_exprs = Interp.denote_exp tid store_data in
           let tainted = List.exists store_exprs ~f:is_tainted in
           if tainted
           then
             let desc = "tainted value reaching store" in
             let subname = Sub.name sub in
             let alerts = Alert.Set.singleton @@ build_alert ~tid ~desc ~subname in
             { empty_res with warns = alerts }
           else empty_res
        | _ -> empty_res)
    | _ -> empty_res
end
