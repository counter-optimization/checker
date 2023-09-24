open Core_kernel
open Bap.Std
open Graphlib.Std
open Monads.Std

module Theory = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

module Cfg = Bap.Std.Graphs.Cfg
module IrCfg = Bap.Std.Graphs.Ir

module CC = Calling_context

type edge = (Tid.t * Tid.t * bool)

type edges = edge list

type cc_edge = (CC.t * CC.t * bool)

type cc_edges = cc_edge list

type tidmap = Blk.elt Tid_map.t

type jmp_taken = Never | Always | Maybe

module Callees = struct
  module S = Set.Make_binable_using_comparator(Sub)
  include S
end

module T = struct
  type t = { tidmap : tidmap;
             callees : Callees.t }
end
include T

module ST = struct
  include Monad.State.T1(T)(Monad.Ident)
  include Monad.State.Make(T)(Monad.Ident) 
end
open ST.Syntax

let from_ : edge -> Tid.t = function
  | (from', to', _is_interproc) -> from'

let to_ : edge -> Tid.t = function
  | (from', to', _is_interproc) -> to'

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
      | `Jmp j -> Format.printf "iter_insns--Jmp: %s\n%!" @@ Jmp.to_string j
    in
    Seq.iter insns ~f:print_insn
  in

  let () = Format.printf "nodes are:\n%!" in
  Seq.iter nodes ~f:print_sub_defs

let to_cc_edge ((from', to', is_interproc) : edge) : cc_edge =
  let c = Calling_context.of_tid in
  (c from', c to', is_interproc)

let print_edge (from', to', is_interproc) : unit =
  let from_s = Tid.to_string from' in
  let to_s = Tid.to_string to' in
  printf "(%s, %s, %B)\n%!" from_s to_s is_interproc

let empty : t = { tidmap = Tid_map.empty;
                  callees = Callees.empty }

let init tidmap : t = { empty with tidmap = tidmap }

let merge (st1 : t) (st2 : t) : t =
  { tidmap = Tid_map.merge st1.tidmap st2.tidmap;
    callees = Callees.union st1.callees st2.callees}

let update_tidmap other : unit ST.t =
  ST.update @@ fun st ->
  { st with tidmap = Tid_map.merge st.tidmap other }

let add_callee callee : unit ST.t =
  ST.update @@ fun st ->
  { st with callees = Callees.add st.callees callee }

let insns_of_node n idx_st =
  let elts = Blk.elts @@ Graphs.Ir.Node.label n in
  Seq.filter elts ~f:(function
    | `Def d ->
      let tid = Term.tid d in
      let part_of_indexing = Idx_calculator.is_part_of_idx_insn idx_st tid in
      (* let () = if part_of_indexing *)
      (*          then *)
      (*            printf "in Edge_builder.insns_of_node: skipping idx insn def: %a\n%!" Def.ppo d *)
      (*          else *)
      (*            printf "in Edge_builder.insns_of_node: keeping def: %a\n%!" Def.ppo d in *)
      not part_of_indexing
    | _ -> true)

let get_ret_insn_tid sub_nodes idx_st =
  let num = Seq.length sub_nodes in
  let last_node = Seq.nth_exn sub_nodes (num - 1) in
  let insns = insns_of_node last_node idx_st in
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

let first_insn_of_blk b =
  let insns = Blk.elts b in
  match Seq.hd insns with
  | Some i -> i
  | None -> failwith "In first_insn_of_blk, couldn't find first insn"

let first_insn_of_blk_tid blk_tid sub =
  match Term.find blk_t sub blk_tid with
  | Some blk -> first_insn_of_blk blk
  | None ->
    let tid_s = Tid.to_string blk_tid in
    failwith @@ sprintf "Couldn't get first insn of blk with tid %s" tid_s

let first_blk_of_subnodes subnodes =
  match Seq.hd subnodes with
  | Some n ->
    Graphs.Ir.Node.label n
  | None ->
    failwith "in first_blk_of_sub, couldn't find first blk"

let first_insn_tid_of_sub sub : Tid.t =
  let blks = Term.enum blk_t sub in
  let first_blk = match Seq.hd blks with
    | Some blk -> blk
    | None ->
      let err = sprintf "Couldn't get first blk of sub in first_insns_tid_of_sub" in
      failwith err
  in
  let first_insn = match Blk.elts first_blk |> Seq.hd with
    | Some insn -> insn
    | None ->
      let err = sprintf "Couldn't get first insn of first blk in first_insns_tid_of_sub" in
      failwith err
  in
  Tid_map.tid_of_elt first_insn

let first_insns_of_subnodes subnodes =
  first_blk_of_subnodes subnodes
  |> first_insn_of_blk

let sub_of_tid tid proj : sub Term.t =
  let prog = Project.program proj in
  let sub = Term.find sub_t prog tid in
  match sub with
  | Some sub -> sub
  | None -> failwith "Didn't find sub with that tid in the program"

let last_insn_of_sub sub idx_st : Blk.elt =
  let irg = Sub.to_cfg sub in
  let rev_nodes = Graphlib.postorder_traverse (module Graphs.Ir) irg in
  let last_node = match Seq.hd rev_nodes with
    | Some n -> n
    | None ->
      begin
        let sub_name = Sub.name sub in
        let err_s = sprintf "Couldn't get last node of sub %s in last_insns_of_sub" sub_name in
        failwith err_s
      end in
  let last_node_insns = insns_of_node last_node idx_st in
  let num_insns = Seq.length last_node_insns in
  Seq.nth_exn last_node_insns (num_insns - 1)

let jmp_taken_when (j : jmp term) : jmp_taken =
  let bil_cnd = Jmp.cond j in
  match bil_cnd with
  | Bil.Int w ->
    let cnd_bw = Word.bitwidth w in
    let zero = Word.zero cnd_bw in
    if Word.equal w zero
    then Never
    else Always
  | _ -> Maybe

let is_no_fallthrough_jmp : Blk.elt -> bool = function
  | _ -> false

let edges_of_jump j sub nodes proj idx_st : edges ST.t =
  let fromtid = Term.tid j in
  let ret_insn_tid = get_ret_insn_tid nodes idx_st in
  match jmp_taken_when j with
  | Never -> ST.return []
  | _ ->
    begin
      match Jmp.kind j with
      | Goto (Direct totid) ->
        let first_insn = first_insn_of_blk_tid totid sub in
        let first_insns_tid = Tid_map.tid_of_elt first_insn in
        ST.return [(fromtid, first_insns_tid, false)]
      | Goto _ when Tid.equal fromtid ret_insn_tid ->
        ST.return []
      | Goto (Indirect _expr) ->
        failwith "Indirect jumps not handled in edge building yet (outer goto)"
      | Call c ->
        begin
          let call_target = Call.target c in
          match call_target with
          | Direct totid ->
            begin
              let callee = sub_of_tid totid proj in
              add_callee callee >>= fun () ->
              let actual_totid = first_insn_tid_of_sub callee in
              ST.return [(fromtid, actual_totid, true)] >>= fun es ->
              let last_elt = last_insn_of_sub callee idx_st in
              let last_elt_tid = Tid_map.tid_of_elt last_elt in
              let return_tid = match Call.return c with
                | Some (Direct l) ->
                  begin
                    let ret_insn = first_insn_of_blk_tid l sub in
                    Tid_map.tid_of_elt ret_insn
                  end
                | Some (Indirect _exp) -> failwith "in interproc edge building, can't handle indirect label..."
                | None -> failwith "in interproc edge building, call doesn't return"
              in
              ST.return @@ List.cons (last_elt_tid, return_tid, true) es 
            end
          | Indirect _expr when Tid.equal fromtid ret_insn_tid ->
            ST.return []
          | Indirect _expr ->
            failwith "Indirect jumps not handled in edge building yet (call)"
        end
      | Int (_, _) -> ST.return []
      | Ret _ -> ST.return []
    end

let edges_of_jump_intraproc j sub nodes proj : edges ST.t =
  let fromtid = Term.tid j in
  match jmp_taken_when j with
  (* | Never -> ST.return [] *)
  | _ ->
    match Jmp.kind j with
    | Goto (Direct totid) ->
      let first_insn = first_insn_of_blk_tid totid sub in
      let first_insns_tid = Tid_map.tid_of_elt first_insn in
      ST.return [(fromtid, first_insns_tid, false)]
    | Call target ->
      let retlabel = Call.return target in
      begin
        match retlabel with
        | Some (Direct totid) ->
          let first_insn_of_ret_blk = first_insn_of_blk_tid totid sub in
          let first_insns_tid = Tid_map.tid_of_elt first_insn_of_ret_blk in
          ST.return [(fromtid, first_insns_tid, false)]
        | _ -> ST.return []
      end
    | Goto _ -> ST.return []
    | Int (_, _) -> ST.return []
    | Ret _ -> ST.return []

let get_jmp_edges insns sub nodes proj =
  let edges insn = match insn with
    | `Jmp j -> edges_of_jump_intraproc j sub nodes proj
    | _ -> ST.return [] in
  Seq.fold insns ~init:(ST.return []) ~f:(fun st insn ->
    st >>= fun e_st ->
    edges insn >>= fun es ->
    ST.return @@ List.append es e_st)

let edges_of_insns insns sub nodes proj : edges ST.t =
  let keep_edge no_fallthrough_jmps (from_, to_, _) : bool =
    not @@ Tidset.mem no_fallthrough_jmps from_ in
  let tid_list = Seq.map insns ~f:Common.elt_to_tid
                 |> Seq.to_list in
  let no_fallthrough_jmps = Seq.filter insns ~f:is_no_fallthrough_jmp
                            |> Seq.map ~f:Common.elt_to_tid
                            |> Seq.to_list
                            |> Tidset.of_list in
  let adjacent_tids, _ = List.zip_with_remainder tid_list (List.tl_exn tid_list) in
  let fallthroughs = List.map adjacent_tids ~f:(fun (from, t) -> (from, t, false)) in
  let fallthroughs = List.filter fallthroughs ~f:(keep_edge no_fallthrough_jmps) in
  get_jmp_edges insns sub nodes proj >>= fun jmp_edges ->
  ST.return @@ List.append jmp_edges fallthroughs

(** only remove dead defs from the CFG (edges). 
    defs only have one out edge, but can have
    multiple in edges like if it is a jump target. 
    additionally, a def can have 0 in edges and 0 out
    edges 

    if both out and ins are present: reroute ins to the out
    if out pres and no ins, then drop the out
    if ins pres and no out, then drop the ins
    shouldn't be the case that no ins and no outs are present

    to reroute, map the ins to have to_ as out edge to_
    and remove all ins and outs from the edges

*)
let remove_dead_defs (edges : cc_edges) (dead : Tidset.t)
  : cc_edges =
  let t = Calling_context.to_insn_tid in
  let c = Calling_context.of_tid in
  let id : 'a. 'a -> 'a = fun x -> x in
  let eq = Tid.equal in
  let edge_eq (f1, t1, _) (f2, t2, _) = eq f1 f2 && eq t1 t2 in
  let rec get_out edges d =
    match edges with
    | (from_, to_, b) :: edges' when eq from_ d ->
      [(from_, to_, b)]
    | e :: edges' -> get_out edges' d
    | [] -> []
  in
  let rec get_ins es d k =
    match es with
    | (from_, to_, b) :: es' when eq to_ d ->
      get_ins es' d (fun res ->
        (from_, to_, b) :: res)
    | e :: es' -> get_ins es' d k
    | [] -> k []
  in
  let remove_edges es toremove =
    List.filter es ~f:(fun edge ->
      not (List.mem toremove edge ~equal:edge_eq))
  in
  let get_new_edges ins out =
    match ins, out with
    | [], o :: os -> []
    | i :: is, [] -> []
    | [], [] -> failwith "[RemoveDeadEdges] no ins or outs"
    | _, (_, newto, _) :: os ->
      List.map ins ~f:(fun (from_, _, b) ->
        (from_, newto, b))
  in
  let cc_to_tid_edge (from_, to_, b) = (t from_, t to_, b) in
  let tid_edges = List.map edges ~f:cc_to_tid_edge in
  let deadseq = Tidset.to_sequence dead in
  Seq.fold deadseq ~init:tid_edges ~f:(fun es d ->
    let out = get_out es d in
    let ins = get_ins es d id in
    let ins_and_outs = List.append out ins in
    let es = remove_edges es ins_and_outs in
    let newedges = get_new_edges ins out in
    List.append newedges es)
  |> List.map ~f:(fun (from_, to_, b) ->
    (c from_, c to_, b))

let get_builder_for_sub sub proj idx_st : edges ST.t =
  let irg = Sub.to_cfg sub in
  let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) irg in
  let lead_tidmap = Tid_map.t_of_sub sub in
  let init_state = ST.put (init lead_tidmap) >>= fun () ->
    ST.return [] in
  Seq.fold nodes ~init:init_state
    ~f:(fun st node ->
      st >>= fun prev_edges -> 
      let insns = insns_of_node node idx_st in
      edges_of_insns insns sub nodes proj >>= fun new_edges ->
      ST.return @@ List.append new_edges prev_edges)

let run (outermost : Sub.t) (proj : Project.t) idx_st : (edges * tidmap) =
  let rec loop worklist seen edges st : edges * t =
    let eff_worklist = Callees.diff worklist seen
                       |> Callees.to_list
    in
    match List.hd eff_worklist with
    | None -> (edges, st)
    | Some sub when Callees.mem seen sub ->
      let worklist = Callees.remove worklist sub in
      loop worklist seen edges st
    | Some sub ->
      let builder = get_builder_for_sub sub proj idx_st in
      let (callee_edges, callee_st) = ST.run builder st in
      let edges = List.append callee_edges edges in
      let st = merge st callee_st in
      let worklist = Callees.union worklist st.callees in
      let worklist = Callees.remove worklist sub in
      let seen = Callees.add seen sub in
      loop worklist seen edges st
  in
  let init_seen = Callees.empty in
  let init_worklist = Callees.singleton outermost in
  let init_edges = [] in
  let init_st = empty in
  let (edges, st) = loop init_worklist init_seen init_edges init_st in
  match edges with
  | [] ->
    let err_str = sprintf "in Edge_builder.run, didn't find edges for sub %s" (Sub.name outermost) in
    failwith err_str
  | _ -> (edges, st.tidmap)

let run_one (sub : Sub.t) (proj : Project.t) (idx_st : Idx_calculator.t) : (edges * tidmap) =
  let builder = get_builder_for_sub sub proj idx_st in
  let (edges, st) = ST.run builder empty in
  edges, st.tidmap
