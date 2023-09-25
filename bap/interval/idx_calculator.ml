 open Core_kernel
open Bap.Std
open Graphlib.Std

module Cfg = Bap.Std.Graphs.Cfg
module IrCfg = Bap.Std.Graphs.Ir

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

module Env = Map.Make_binable_using_comparator(String)

type state = {
  subname : string;
  sub : sub term;
  blks : blk term Seq.t;
  idx_map : int Tid_map.t;
  idx_insn_tids : Set.M(Tid).t
}

type t = state

let rec is_idx_insn ?(in_sub : bool = false) ?(in_plus : bool = false) exp =
  match exp with
  | Bil.BinOp (Bil.MINUS, left, right) ->
    (match left with
     | Bil.Var _ -> true
     | _ -> false)
    &&
    is_idx_insn ~in_sub:true right
  | Bil.BinOp (Bil.PLUS, left, right) ->
    in_sub
    &&
    (match left with
     | Bil.Int _ -> true
     | Bil.Var _ -> true
     | _ -> false)
    &&
    is_idx_insn ~in_sub ~in_plus:true right
  | Bil.Cast (cast, sz, subexp) ->
    in_sub
    &&
    in_plus
    &&
    (match subexp with
     | Bil.Var v -> String.Caseless.equal "cf" @@ Var.name v
     | _ -> false)
  | _ -> false

let elt_is_idx_insn = function
  | `Def d ->
    let varname = Var.name @@ Def.lhs d in
    if String.Caseless.equal varname "r11"
    then is_idx_insn @@ Def.rhs d 
    else false
  | _ -> false

let get_idx_from_idx_insn_rhs exp (env : word Env.t) tid : int =
  let fail () =
    failwith @@ sprintf "In Idx_calculator, get_idx_from_idx_insn_rhs: unknown idx insn format for tid: %a" Tid.pps tid in
  match exp with
  | Bil.BinOp (Bil.MINUS, left, (Bil.BinOp (Bil.PLUS, idx_holder, _))) ->
    let idx_word = match idx_holder with
      | Bil.Int w -> w
      | Bil.Var v ->
        let varname = Var.name v in
        (match Env.find env varname with
         | Some w -> w
         | None -> fail ())
      | _ -> fail ()
    in
    (match Word.to_int idx_word with
     | Ok i -> i
     (* let () = printf "in Idx_calculator, int: %d, word: %a\n%!" i Word.ppo idx_word in *)
     | Error _ -> fail ())
  | _ -> fail ()

let try_assigning_consts (lhs : Var.t) (rhs : Bil.exp) (env : word Env.t) : word Env.t =
  match rhs with
  | Bil.Int w ->
    let varname = Var.name lhs in
    Env.set ~key:varname ~data:w env
  | _ -> env

let build_idx_map_for_blk basic_blk idx_map : (idx Tid_map.t * Set.M(Tid).t) =
  let start_idx : idx option = None in
  let rec loop ?(is_idx_insn_flags : bool = false) defterms cur_idx idx_map idx_insn_tids env =
    if Seq.is_empty defterms
    then (idx_map, idx_insn_tids)
    else
      let curdef = Seq.hd_exn defterms in
      (* let () = printf "Processing def term: %a\n%!" Def.ppo curdef in *)
      let rest_defs = Option.value_exn (Seq.tl defterms) in
      let lhs = Def.lhs curdef in 
      let assigned_var = Var.name lhs in
      (* let () = printf "Var assigned is: %s\n%!" assigned_var in *)
      (* let () = if Option.is_some cur_idx *)
      (*          then printf "Cur idx is %d\n%!" @@ Option.value_exn cur_idx *)
      (*          else printf "No cur idx\n%!" *)
      (* in *)
      let rhs = Def.rhs curdef in
      let env = try_assigning_consts lhs rhs env in
      let current_tid = Term.tid curdef in
      if String.Caseless.equal "r11" assigned_var && is_idx_insn rhs
      then
        let cur_idx = get_idx_from_idx_insn_rhs rhs env current_tid in
        let idx_insn_tids = Set.add idx_insn_tids current_tid in
        let is_idx_insn_flags = true in
        (* let () = printf "New cur_idx is: %d\n%!" cur_idx in *)
        (* let () = printf "Tid %a is an idx insn\n%!" Tid.ppo current_tid in *)
        (* let () = printf "Setting is_idx_insn_flags %B\n%!" is_idx_insn_flags in *)
        loop ~is_idx_insn_flags rest_defs (Some cur_idx) idx_map idx_insn_tids env
      else
        let cur_is_flag = Common.AMD64SystemVABI.var_name_is_flag assigned_var in
        let is_flag_of_idx_insn = is_idx_insn_flags && cur_is_flag in
        (* let () = printf "not sbb insns, is it a flag?: %B\n%!" cur_is_flag in *)
        (* let () = printf "not sbb insns, is it a flag of idx insn?: %B\n%!" is_flag_of_idx_insn in *)
        if is_flag_of_idx_insn
        then
          let idx_insn_tids = Set.add idx_insn_tids current_tid in
          loop ~is_idx_insn_flags:true rest_defs cur_idx idx_map idx_insn_tids env
        else
          let idx_map = match cur_idx with
            | Some insn_idx -> Tid_map.set idx_map ~key:current_tid ~data:insn_idx 
            | None -> idx_map in
          loop ~is_idx_insn_flags:false rest_defs cur_idx idx_map idx_insn_tids env
  in
  let defterms = Term.enum def_t basic_blk in
  let init_env : word Env.t = Env.empty in
  let idx_insn_tids = Set.empty (module Tid) in
  loop defterms start_idx idx_map idx_insn_tids init_env

let build_idx_map_for_blks (blks : blk term Seq.t) : (idx Tid_map.t * Set.M(Tid).t)=
  let idx_map : idx Tid_map.t = Tid_map.empty in
  let idx_insn_tids = Set.empty (module Tid) in
  Seq.fold blks ~init:(idx_map, idx_insn_tids)
    ~f:(fun (idx_map, idx_insn_tids) blk ->
      let idx_map, idx_insn_tids' = build_idx_map_for_blk blk idx_map in
      idx_map, Set.union idx_insn_tids' idx_insn_tids
    )

let rpo_of_sub sub : blk term Seq.t =
  let cfg = Sub.to_cfg sub in
  let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) cfg in
  let blks = Seq.map nodes ~f:Graphs.Ir.Node.label in
  blks

let build sub : state =
  let name = Sub.name sub in
  (* let () = printf "Building lut, idx_map, for sub: %s\n%!" name in *)
  let blks = rpo_of_sub sub in
  let idx_map, idx_insn_tids = build_idx_map_for_blks blks in
  (* let () = printf "idx_map is:\n%!"; *)
  (*          Tid_map.iteri idx_map ~f:(fun ~key ~data -> *)
  (*              printf "\t%a -> %d\n%!" Tid.ppo key data) *)
  (* in *)
  { subname = name; sub; blks; idx_map; idx_insn_tids }

let is_part_of_idx_insn { idx_insn_tids; _ } tid =
  Set.mem idx_insn_tids tid

let contains_tid (tid : tid) { idx_map; _ }=
  Tid_map.mem idx_map tid

let get_idx tid { idx_map; _ } : idx option =
  Tid_map.find idx_map tid
