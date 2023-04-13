open Core
open Bap.Std
open Graphlib.Std

module Cfg = Bap.Std.Graphs.Cfg
module IrCfg = Bap.Std.Graphs.Ir

type idx = int

type state = {
    subname : string;
    sub : sub term;
    blks : blk term Seq.t;
    idx_map : int Tid_map.t
}

type t = state

let get_exp_as_const_int : Bil.exp -> int option = function
  | Bil.Int w -> (match Word.to_int w with
                  | Ok i -> Some i
                  | Error _ ->
                     let err_msg = sprintf "Couldn't convert word %a in idx_calculator" Word.pps w in
                     failwith err_msg)
  | _ -> None

let build_idx_map_for_blk basic_blk idx_map : idx Tid_map.t =
  let start_idx : idx option = None in
  let rec loop defterms cur_idx idx_map =
    if Seq.is_empty defterms
    then idx_map
    else
      let curdef = Seq.hd_exn defterms in
      let rest_defs = Option.value_exn (Seq.tl defterms) in
      let assigned_var = Var.name @@ Def.lhs curdef in
      if String.Caseless.equal "r11" assigned_var
      then
        let rhs = Def.rhs curdef in
        let cur_idx = get_exp_as_const_int rhs in
        loop rest_defs cur_idx idx_map
      else
        let current_tid = Term.tid curdef in
        let idx_map = match cur_idx with
          | Some insn_idx -> Tid_map.set idx_map ~key:current_tid ~data:insn_idx 
          | None -> idx_map
        in
        loop rest_defs cur_idx idx_map
  in
  let defterms = Term.enum def_t basic_blk in
  loop defterms start_idx idx_map

let build_idx_map_for_blks (blks : blk term Seq.t) : idx Tid_map.t =
  let idx_map : idx Tid_map.t = Tid_map.empty in
  Seq.fold blks ~init:idx_map ~f:(fun idx_map blk ->
      build_idx_map_for_blk blk idx_map)

let rpo_of_sub sub : blk term Seq.t =
  let cfg = Sub.to_cfg sub in
  let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) cfg in
  let blks = Seq.map nodes ~f:Graphs.Ir.Node.label in
  blks

let build sub : state =
  let name = Sub.name sub in
  let blks = rpo_of_sub sub in
  let idx_map = build_idx_map_for_blks blks in
  { subname = name; sub; blks; idx_map }

let contains_tid (tid : tid) { idx_map; _ }=
  Tid_map.mem idx_map tid

let get_idx tid { idx_map; _ } : idx option =
  Tid_map.find idx_map tid
