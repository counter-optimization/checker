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
    | None -> failwith "[DmpChecker] Couldn't extract taint information out of product domain"

  let get_bv : N.t -> Abstract_bitvector.t =
    match N.get Abstract_bitvector.key with
    | Some f -> f
    | None -> failwith "[DmpChecker] Couldn't extract abs bitvector info out of product domain"

  let is_tainted n =
    match get_taint n with
    | Checker_taint.Analysis.Notaint -> false
    | Checker_taint.Analysis.Taint -> true

  let build_alert ~tid ~desc ~subname ~term : Alert.t =
    let tid = Some tid in
    let term = Some term in
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
    { tid; desc; left_val; right_val; term;
      reason; sub_name; problematic_operands;
      opcode; addr; rpo_idx; flags_live_in;
      flags_live; is_live }

  (* When is a store unsafe?: 
     if it is a store of tainted data to a pointer without 
     bit 60 set in the pointer AND it is not between a LAHF 
     and SAHF *)
  let check_elt sub tid lahf_sahf elt
    : Alert.Set.t Common.checker_res =
    let could_be_tainted = List.exists ~f:is_tainted in
    let bit_60_unset exp =
      let module BV = Abstract_bitvector in
      let exp_bv = get_bv exp in
      let bit60 = BV.with_bit_60_set in
      BV.contains BV.b0 (BV.logand exp_bv bit60) in
    let could_have_bit_60_unset =
      List.exists ~f:bit_60_unset in
    let empty_stats = Common.EvalStats.init in
    let empty_res : Alert.Set.t Common.checker_res = {
      warns = Alert.Set.empty;
      cs_stats = empty_stats;
      ss_stats = empty_stats
    } in
    if Lahf_and_sahf.tid_part_of_transform lahf_sahf tid
    then empty_res
    else
    match elt with
    | `Def d ->
      (match Def.rhs d with
       | Bil.Store (_mem, idx, store_data, _endian, _size) ->
         let denoted_store_exp = Interp.denote_exp tid store_data in
         let denoted_pointer_exp = Interp.denote_exp tid idx in
         if could_be_tainted denoted_store_exp &&
            could_have_bit_60_unset denoted_pointer_exp
         then
           let desc = "tainted value reaching store without 60th bit set" in
           let subname = Sub.name sub in
           let alerts = Alert.Set.singleton @@
             build_alert ~tid ~desc ~subname ~term:d in
           { empty_res with warns = alerts }
         else empty_res
       | _ -> empty_res)
    | _ -> empty_res
end
