open Core_kernel
open Bap.Std
open Graphlib.Std

module Cfg = Bap.Std.Graphs.Cfg
module IrCfg = Bap.Std.Graphs.Ir
module KB = Bap_knowledge.Knowledge

module T = struct
  type state = {
    idxs : Tid.Set.t;
    st : int Tid.Map.t;
  }

  type t = state
end

module Pass = struct
  open Option.Monad_infix

  type state = Tid.Set.t Int.Map.t
                 
  type t = {
    mutable st : state;
    mutable intids : Tid.Set.t;
  }

  type _ Uc_single_shot_pass.key +=
      Key : t Uc_single_shot_pass.key
             
  let is_r11 = String.Caseless.is_substring
                 ~substring:"r11"

  let is_sbb = String.Caseless.is_prefix
                 ~prefix:"sbbq"

  let rn = Reg.name

  let default () = {
    st = Int.Map.empty;
    intids = Tid.Set.empty;
  }

  let try_get_idx dt =
    Term.get_attr dt Disasm.insn >>= fun sema ->
    let name = Insn.name sema in
    if String.Caseless.is_substring name ~substring:"sbb"
    then
      let operands = Insn.ops sema in
      let dst = Array.unsafe_get operands 0 in
      let src = Array.unsafe_get operands 1 in
      let idx = Array.unsafe_get operands 2 in
      match dst, src, idx with
      | Reg dst, Reg src, Imm idx when is_r11 (rn dst) &&
                                       is_r11 (rn src) ->
        Imm.to_int idx
      | _ -> None
    else None

  let get_ir_tids dt =
    Term.get_attr dt Disasm.insn >>= fun sema ->
    let blks = KB.Value.get Term.slot sema in
    Some (Common.tids_of_blks blks)

  let ondef dt ({st;intids} as env) =
    let tid = Term.tid dt in
    if Tid.Set.mem intids tid
    then env
    else
      let res = try_get_idx dt >>= fun idx ->
        get_ir_tids dt >>= fun tids ->
        Some (idx, tids)
      in
      match res with
      | Some (idx, tids) ->
        env.st <- Int.Map.set st ~key:idx ~data:tids;
        env.intids <- tids;
        env
      | None ->
        env.intids <- Tid.Set.empty;
        env

  let onjmp j st = st
  let onphi p st = st

  let set (idx : int) tids ~(succ : tid -> tid Seq.t) =
    let succs = Tid.Set.fold tids ~init:[] ~f:(fun ss t ->
      (succ t |> Seq.to_list) @ ss)
    in
    List.fold succs ~init:Tid.Map.empty ~f:(fun m succ ->
      Tid.Map.set m ~key:succ ~data:idx)

  let get_state ~succ {st} : T.t =
    let init = Tid.Map.empty in
    let idxs = Int.Map.data st |>
               List.fold ~init:Tid.Set.empty ~f:Tid.Set.union
    in
    let st = Int.Map.fold st ~init ~f:(fun ~key ~data m ->
      set key data ~succ)
    in
    {idxs;st}
end

(**
   VAR CASE:
   000007ba: #12582866 := 3
   000007be: #12582865 := R11
   000007c4: R11 := #12582865 - #12582866 + pad:64[CF]

   INT CASES:
   ZERO CASE:
   ((bap:insn ((SBB64ri32 R11 R11 0x0)))
   (bap:mem ("401110: 49 81 db 00 00 00 00"))
   (bap:bil-code
   "{
   #12582879 := R11
   R11 := #12582879 - pad:64[CF]
   OF := high:1[#12582879 & (#12582879 ^ R11)]
   CF := #12582879 < pad:64[CF] | pad:64[CF] < 0
   AF := 0x10 = (0x10 & (R11 ^ #12582879))
   PF :=
   ~low:1[let $3 = R11 >> 4 ^ R11 in let $4 = $3 >> 2 ^ $3 in $4 >> 1 ^
   $4]
   SF := high:1[R11]
   ZF := 0 = R11
   }")

   NON-ZERO CASE:
   (0x40111b
   ((bap:insn ((SBB64ri32 R11 R11 0x1)))
   (bap:mem ("40111b: 49 81 db 01 00 00 00"))
   (bap:bil-code
   "{
   #12582875 := R11
   R11 := #12582875 - 1 + pad:64[CF]
   OF := high:1[(1 ^ #12582875) & (#12582875 ^ R11)]
   CF := #12582875 < 1 + pad:64[CF] | 1 + pad:64[CF] < 1
   AF := 0x10 = (0x10 & (R11 ^ 1 ^ #12582875))
   PF :=
   ~low:1[let $3 = R11 >> 4 ^ R11 in let $4 = $3 >> 2 ^ $3 in $4 >> 1 ^
   $4]
   SF := high:1[R11]
   ZF := 0 = R11
   }")
*)

type idx = int

(* module Env = Map.Make_binable_using_comparator(String) *)

(* type state = { *)
(*   subname : string; *)
(*   sub : sub term; *)
(*   blks : blk term Seq.t; *)
(*   idx_map : int Tid_map.t; *)
(*   idx_insn_tids : Set.M(Tid).t *)
(* } *)

(* type t = state *)

include T

let contains_tid tid {st;_} = Tid.Map.mem st tid

let is_part_of_idx_insn {idxs;_} tid = Tid.Set.mem idxs tid

let get_idx tid {st;_} = Tid.Map.find st tid

(* let rec is_idx_insn ?(in_sub : bool = false) ?(in_plus : bool = false) exp = *)
(*   match exp with *)
(*   | Bil.BinOp (Bil.MINUS, left, right) -> *)
(*     (match left with *)
(*      | Bil.Var _ -> true *)
(*      | _ -> false) *)
(*     && *)
(*     is_idx_insn ~in_sub:true right *)
(*   | Bil.BinOp (Bil.PLUS, left, right) -> *)
(*     in_sub *)
(*     && *)
(*     (match left with *)
(*      | Bil.Int _ -> true *)
(*      | Bil.Var _ -> true *)
(*      | _ -> false) *)
(*     && *)
(*     is_idx_insn ~in_sub ~in_plus:true right *)
(*   | Bil.Cast (cast, sz, subexp) -> *)
(*     in_sub *)
(*     && *)
(*     in_plus *)
(*     && *)
(*     (match subexp with *)
(*      | Bil.Var v -> String.Caseless.equal "cf" @@ Var.name v *)
(*      | _ -> false) *)
(*   | _ -> false *)

(* let elt_is_idx_insn = function *)
(*   | `Def d -> *)
(*     let varname = Var.name @@ Def.lhs d in *)
(*     if String.Caseless.equal varname "r11" *)
(*     then is_idx_insn @@ Def.rhs d  *)
(*     else false *)
(*   | _ -> false *)

(* let get_idx_from_idx_insn_rhs exp (env : word Env.t) tid : int = *)
(*   let fail () = *)
(*     failwith @@ sprintf "In Idx_calculator, get_idx_from_idx_insn_rhs: unknown idx insn format for tid: %a" Tid.pps tid in *)
(*   match exp with *)
(*   | Bil.BinOp (Bil.MINUS, left, (Bil.BinOp (Bil.PLUS, idx_holder, _))) -> *)
(*     let idx_word = match idx_holder with *)
(*       | Bil.Int w -> w *)
(*       | Bil.Var v -> *)
(*         let varname = Var.name v in *)
(*         (match Env.find env varname with *)
(*          | Some w -> w *)
(*          | None -> fail ()) *)
(*       | _ -> fail () *)
(*     in *)
(*     (match Word.to_int idx_word with *)
(*      | Ok i -> i *)
(*      (\* let () = printf "in Idx_calculator, int: %d, word: %a\n%!" i Word.ppo idx_word in *\) *)
(*      | Error _ -> fail ()) *)
(*   | _ -> fail () *)

(* let try_assigning_consts (lhs : Var.t) (rhs : Bil.exp) (env : word Env.t) : word Env.t = *)
(*   match rhs with *)
(*   | Bil.Int w -> *)
(*     let varname = Var.name lhs in *)
(*     Env.set ~key:varname ~data:w env *)
(*   | _ -> env *)

(* let build_idx_map_for_blk basic_blk idx_map : (idx Tid_map.t * Set.M(Tid).t) = *)
(*   let start_idx : idx option = None in *)
(*   let rec loop ?(is_idx_insn_flags : bool = false) defterms cur_idx idx_map idx_insn_tids env = *)
(*     if Seq.is_empty defterms *)
(*     then (idx_map, idx_insn_tids) *)
(*     else *)
(*       let curdef = Seq.hd_exn defterms in *)
(*       let rest_defs = Option.value_exn (Seq.tl defterms) in *)
(*       let lhs = Def.lhs curdef in  *)
(*       let assigned_var = Var.name lhs in *)
(*       let rhs = Def.rhs curdef in *)
(*       let env = try_assigning_consts lhs rhs env in *)
(*       let current_tid = Term.tid curdef in *)
(*       if String.Caseless.equal "r11" assigned_var && is_idx_insn rhs *)
(*       then *)
(*         let cur_idx = get_idx_from_idx_insn_rhs rhs env current_tid in *)
(*         let idx_insn_tids = Set.add idx_insn_tids current_tid in *)
(*         let is_idx_insn_flags = true in *)
(*         loop ~is_idx_insn_flags rest_defs (Some cur_idx) idx_map idx_insn_tids env *)
(*       else *)
(*         let cur_is_flag = Common.AMD64SystemVABI.var_name_is_flag assigned_var in *)
(*         let is_flag_of_idx_insn = is_idx_insn_flags && cur_is_flag in *)
(*         if is_flag_of_idx_insn *)
(*         then *)
(*           let idx_insn_tids = Set.add idx_insn_tids current_tid in *)
(*           loop ~is_idx_insn_flags:true rest_defs cur_idx idx_map idx_insn_tids env *)
(*         else *)
(*           let idx_map = match cur_idx with *)
(*             | Some insn_idx -> Tid_map.set idx_map ~key:current_tid ~data:insn_idx  *)
(*             | None -> idx_map in *)
(*           loop ~is_idx_insn_flags:false rest_defs cur_idx idx_map idx_insn_tids env *)
(*   in *)
(*   let defterms = Term.enum def_t basic_blk in *)
(*   let init_env : word Env.t = Env.empty in *)
(*   let idx_insn_tids = Set.empty (module Tid) in *)
(*   loop defterms start_idx idx_map idx_insn_tids init_env *)

(* let build_idx_map_for_blks (blks : blk term Seq.t) : (idx Tid_map.t * Set.M(Tid).t)= *)
(*   let idx_map : idx Tid_map.t = Tid_map.empty in *)
(*   let idx_insn_tids = Set.empty (module Tid) in *)
(*   Seq.fold blks ~init:(idx_map, idx_insn_tids) *)
(*     ~f:(fun (idx_map, idx_insn_tids) blk -> *)
(*       let idx_map, idx_insn_tids' = build_idx_map_for_blk blk idx_map in *)
(*       idx_map, Set.union idx_insn_tids' idx_insn_tids *)
(*     ) *)

(* let rpo_of_sub sub : blk term Seq.t = *)
(*   let cfg = Sub.to_cfg sub in *)
(*   let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) cfg in *)
(*   let blks = Seq.map nodes ~f:Graphs.Ir.Node.label in *)
(*   blks *)

(* let build sub : state = *)
(*   let name = Sub.name sub in *)
(*   let blks = rpo_of_sub sub in *)
(*   let idx_map, idx_insn_tids = build_idx_map_for_blks blks in *)
(*   { subname = name; sub; blks; idx_map; idx_insn_tids } *)

(* let is_part_of_idx_insn { idx_insn_tids; _ } tid = *)
(*   Set.mem idx_insn_tids tid *)

(* let contains_tid (tid : tid) { idx_map; _ }= *)
(*   Tid_map.mem idx_map tid *)

(* let get_idx tid { idx_map; _ } : idx option = *)
(*   Tid_map.find idx_map tid *)
