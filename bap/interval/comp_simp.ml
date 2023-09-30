open Core_kernel
open Bap.Std
open Graphlib.Std
open Monads.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB
module WI = Wrapping_interval
module ABI = Common.AMD64SystemVABI
module SS = Common.SS
module Stats = Common.EvalStats

module Checker(N : Abstract.NumericDomain)
    (Interp : Common.CheckerInterp with type t := N.t) = struct
  type st = {
    tid : tid;
    term : def term;
    subname : string;
  }
  
  let emp = Alert.Set.empty
  let init_st subname tid term = {tid;subname;term}

  let estats_incr_total_considered st = Uc_stats.(incr cs_stats total)
  let estats_incr_taint_pruned st = Uc_stats.(incr cs_stats taint_pruned)
  let estats_incr_interval_pruned st = Uc_stats.(incr cs_stats interval_pruned)
                                         
  let get_intvl : N.t -> Wrapping_interval.t =
    match N.get Wrapping_interval.key with
    | Some f -> f
    | None -> failwith "[CSChkr] Couldn't extract interval from product domain"

  let get_taint : N.t -> Checker_taint.Analysis.t =
    match N.get Checker_taint.Analysis.key with
    | Some f -> f
    | None -> failwith "[CSChkr] Couldn't extract taint from product domain"

  (* increment total considered *)
  let incr_total_considered binop st : unit =
    match binop with
    | Bil.EQ | Bil.NEQ | Bil.LT | Bil.LE | Bil.SLT
    | Bil.SLE | Bil.MOD | Bil.SMOD -> ()
    | _ -> estats_incr_total_considered st

  (* 
     U N U N -> taint pruned
     U N U B -> taint pruned
     U N T N -> interval pruned
     U N T B -> flagged

     U B U N -> taint pruned
     U B U B -> taint pruned
     U B T N -> interval pruned
     U B T B -> flagged

     T N U N -> interval pruned
     T N U B -> interval pruned
     T N T N -> interval pruned
     T N T B -> flagged

     T B U N -> flagged
     T B U B -> flagged
     T B T N -> flagged
     T B T B -> flagged

     flagged if (tl && !left_bad) || (tr && !right_bad)
     taint pruned if (not tl && not tr)
     interval pruned if (tl && not !left_bad) || (tr && not !right_bad)

     sub case:
     flagged if (tr && !right_bad)
     taint_pruned if (not tr)
     interval pruned if (tr && not !right_bad)

     total:
     flagged if (is_sub && tr && !right_bad) || 
     (not is_sub && ((tl && !left_bad) || (tr && !right_bad)))
     taint pruned if (is_sub && not tr) &&
     (not is_sub && not tl && not tr)
     interval pruned if (is_sub && tr && not !right_bad) ||
     (not is_sub && ((tl && not !left_bad) || 
     (tr && not !right_bad)))

     warn if left tainted && left bad
     warn if right tainted && right bad
     incr taint pruned if (left not tainted && right not tainted)
     left tainted && left not bad && right not tainted && right bad
  *)
  let check_binop (binop : Bil.binop) (l : N.t) (r : N.t) (st : st) : Alert.Set.t =
    let safe_bitwidth : WI.t -> int = function
      | Bot -> 1
      | intvl -> WI.bitwidth intvl
    in
    let wl = get_intvl l in
    let wr = get_intvl r in
    let tl = Checker_taint.Analysis.is_tainted @@ get_taint l in
    let tr = Checker_taint.Analysis.is_tainted @@ get_taint r in
    let left_zero = WI.of_int ~width:(safe_bitwidth wl) 0 in
    let right_zero = WI.of_int ~width:(safe_bitwidth wr) 0 in
    let left_one = WI.of_int ~width:(safe_bitwidth wl) 1 in
    let right_one = WI.of_int ~width:(safe_bitwidth wr) 1 in
    let left_all_ones = begin
      let bw = safe_bitwidth wl in
      let ones = Word.ones bw in
      WI.of_word ones
    end
    in
    let right_all_ones = begin
      let bw = safe_bitwidth wr in
      let ones = Word.ones bw in
      WI.of_word ones
    end
    in
    let left_bad = ref false in
    let right_bad = ref false in
    let binop_is_sub = match binop with
      | Bil.MINUS -> true
      | _ -> false
    in
    incr_total_considered binop st;
    let untainted = (not binop_is_sub && not tl && not tr) ||
                    (binop_is_sub && not tr)
    in
    if untainted
    then (estats_incr_taint_pruned st; emp)
    else
      let () = match binop with
        | Bil.PLUS ->
          left_bad := WI.contains left_zero wl;
          right_bad := WI.contains right_zero wr
        | Bil.MINUS ->
          right_bad := WI.contains right_zero wr
        | Bil.TIMES -> 
          left_bad := WI.contains left_zero wl || WI.contains left_one wl;
          right_bad := WI.contains right_zero wr || WI.contains right_one wr
        | Bil.DIVIDE ->
          left_bad := WI.contains left_zero wl;
          right_bad := WI.contains right_one wr
        | Bil.SDIVIDE ->
          left_bad := WI.contains left_zero wl;
          right_bad := WI.contains right_one wr
        | Bil.LSHIFT ->
          left_bad := WI.contains left_zero wl;
          right_bad := WI.contains right_zero wr
        | Bil.RSHIFT -> 
          left_bad := WI.contains left_zero wl;
          right_bad := WI.contains right_zero wr
        | Bil.ARSHIFT ->
          left_bad := WI.contains left_zero wl;
          right_bad := WI.contains right_zero wr
        | Bil.AND ->
          left_bad := WI.contains left_zero wl || WI.contains left_all_ones wl;
          right_bad := WI.contains right_zero wr || WI.contains right_all_ones wr
        | Bil.OR ->
          left_bad := WI.contains left_zero wl || WI.contains left_all_ones wl;
          right_bad := WI.contains right_zero wr || WI.contains right_all_ones wr
        | Bil.XOR ->
          left_bad := WI.contains left_zero wl || WI.contains left_all_ones wl;
          right_bad := WI.contains right_zero wr || WI.contains right_all_ones wr
        | Bil.EQ -> ()
        | Bil.NEQ -> ()
        | Bil.LT -> ()
        | Bil.LE -> ()
        | Bil.SLT -> ()
        | Bil.SLE -> ()
        | Bil.MOD -> ()
        | Bil.SMOD -> ()
      in
      let interval_pruned = (binop_is_sub && tr && not !right_bad) ||
                            (not binop_is_sub &&
                             (tl || tr) &&
                             (not tl || not !left_bad) &&
                             (not tr || not !right_bad))

      in
      if interval_pruned
      then (estats_incr_interval_pruned st; emp)
      else
        let problematic_operands = [] in
        let problematic_operands = List.append (if !left_bad then [0] else []) problematic_operands in
        let problematic_operands = Some (List.append (if !right_bad then [1] else []) problematic_operands) in
        let desc = Bil.string_of_binop binop in
        let left_val = Some (WI.to_string wl) in
        let right_val = Some (WI.to_string wr) in
        let alert : Alert.t = {
          tid = Some st.tid;
          term = Some st.term;
          desc;
          left_val;
          right_val;
          reason = Alert.CompSimp;
          sub_name = Some st.subname;
          problematic_operands;
          opcode = None;
          addr = None;
          rpo_idx = None;
          flags_live = SS.empty;
          flags_live_in = SS.empty;
          is_live = None;
        }
        in
        Alert.Set.singleton alert

  let check_binop_nondet (binop : Bil.binop) (l : N.t list)
        (r : N.t list) (st : st) : Alert.Set.t =
    let operands = List.cartesian_product l r in
    List.fold operands ~init:emp ~f:(fun alerts (lrand, rrand) ->
      Set.union alerts @@ check_binop binop lrand rrand st)

  let rec check_exp (expr : Bil.exp) (st : st) : N.t list * Alert.Set.t =
    let j3 x y z = Set.union x y |> Set.union z in
    match expr with
    | Bil.Load (_, idx, _, _) ->
      (Interp.denote_exp st.tid expr, snd @@ check_exp idx st)
    | Bil.Store (_, idx, data, _, _) ->
      ([N.bot], Set.union
                  (snd @@ check_exp idx st)
                  (snd @@ check_exp data st))
    | Bil.BinOp (op, l, r) ->
      let l, lalerts = check_exp l st in
      let r, ralerts = check_exp r st in
      (Interp.denote_exp st.tid expr,
       j3 lalerts ralerts @@ check_binop_nondet op l r st)
    | Bil.UnOp (_, subexp) ->
      (Interp.denote_exp st.tid expr, snd @@ check_exp subexp st)
    | Bil.Var _ ->
      (Interp.denote_exp st.tid expr, emp)
    | Bil.Int _ ->
      (Interp.denote_exp st.tid expr, emp)
    | Bil.Cast (_, _, subexp) ->
      (Interp.denote_exp st.tid expr, snd @@ check_exp subexp st)
    | Bil.Let (_, e, body) ->
      (Interp.denote_exp st.tid expr,
       Set.union (snd @@ check_exp e st) (snd @@ check_exp body st))
    | Bil.Unknown (_, _) ->
      (Interp.denote_exp st.tid expr, emp)
    | Bil.Ite (i, t, e) ->
      let alerts = j3 (snd @@ check_exp i st)
                     (snd @@ check_exp t st)
                     (snd @@ check_exp e st)
      in
      (Interp.denote_exp st.tid expr, alerts)
    | Bil.Extract (_, _, subexp) ->
      (Interp.denote_exp st.tid expr, snd @@ check_exp subexp st)
    | Bil.Concat (l, r) ->
      (Interp.denote_exp st.tid expr,
       Set.union (snd @@ check_exp l st) (snd @@ check_exp r st))

  let check_elt (subname : string) (tid : tid) (elt : Blk.elt) : Alert.Set.t =
    match elt with
    | `Def d ->
      let defining = Def.lhs d |> Var.name in
      let is_def_of_flag = ABI.var_name_is_flag defining in
      let should_check = not is_def_of_flag in
      if should_check
      then
        let st = init_st subname tid d in    
        let rhs = Def.rhs d in
        let _, alerts = check_exp rhs st in
        alerts
      else emp
    | _ -> emp
end
