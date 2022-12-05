open Core_kernel
open Bap.Std
open Graphlib.Std
open Monads.Std

module KB = Bap_knowledge.Knowledge

module Cfg = Bap.Std.Graphs.Cfg
module IrCfg = Bap.Std.Graphs.Ir

type edge = (Tid.t * Tid.t * bool)

type edges = edge list

type tidmap = Blk.elt Tid_map.t

module T = struct
  type t = { edges: edges; tidmap: tidmap }
end
include T

module ST = struct
  include Monad.State.T1(T)(Monad.Ident)
  include Monad.State.Make(T)(Monad.Ident) 
end
open ST.Syntax

let empty : t = { edges = []; tidmap = Tid_map.empty }
let init tidmap : t = { empty with tidmap = tidmap }

let update_tidmap other : unit ST.t =
  ST.update @@ fun edge_st ->
               let merged = Tid_map.merge edge_st.tidmap other in
               { edge_st with tidmap = merged }

let insns_of_node n = Blk.elts @@ Graphs.Ir.Node.label n

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

let first_insn_of_blk b =
  let insns = Blk.elts b in
  match Seq.hd insns with
  | Some i -> i
  | None -> failwith "In first_insn_of_blk, couldn't find first insn"

let first_insn_of_blk_tid blk_tid sub =
  let the_blk = Term.find_exn blk_t sub blk_tid in
  first_insn_of_blk the_blk

let first_blk_of_subnodes subnodes =
  match Seq.hd subnodes with
  | Some n ->
     Graphs.Ir.Node.label n
  | None ->
     failwith "in first_blk_of_sub, couldn't find first blk"

let first_insns_of_subnodes subnodes =
  first_blk_of_subnodes subnodes
  |> first_insn_of_blk

let sub_of_tid tid proj : sub Term.t =
  let prog = Project.program proj in
  let sub = Term.find sub_t prog tid in
  match sub with
  | Some sub -> sub
  | None -> failwith "Didn't find sub with that tid in the program"

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
  
let edges_of_jump j sub nodes proj : edges ST.t =
  let fromtid = Term.tid j in
  let ret_insn_tid = get_ret_insn_tid nodes in
  match Jmp.kind j with
  | Goto (Direct totid) ->
     let first_insn = first_insn_of_blk_tid totid sub in
     let first_insns_tid = Tid_map.tid_of_elt first_insn in
     ST.return [(fromtid, first_insns_tid, false)]
  | Goto _ when Tid.equal fromtid ret_insn_tid ->
     ST.return []
  | Goto (Indirect _expr) ->
     failwith "Indirect jumps not handled in edge building yet (outer)"
  | Call c ->
     begin
       let call_target = Call.target c in
       match call_target with
       | Direct totid ->
          begin
            let () = printf "totid in call is %s\n" (Tid.to_string totid) in
            let sub = sub_of_tid totid proj in
            let () = printf "sub of that totid is %s\n" (Sub.name sub) in
            let other_tidmap = Tid_map.t_of_sub sub in
            update_tidmap other_tidmap >>= fun () ->

            let first_elt = first_insns_of_subnodes nodes in
            let actual_totid = Tid_map.tid_of_elt first_elt in
            ST.return [(fromtid, actual_totid, true)] >>= fun es ->

            let last_elt = last_insn_of_sub sub in
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
         (* todo: add return edge *)
       | Indirect _expr ->
          failwith "Indirect jumps not handled in edge building yet (call)"
     end
  | Int (_, _)
    | Ret _ -> ST.return []

let get_jmp_edges insns sub nodes proj =
  let edges insn =
    match insn with
    | `Jmp j -> edges_of_jump j sub nodes proj
    | _ -> ST.return []
  in
  Seq.fold insns ~init:(ST.return []) ~f:(fun st insn ->
      st >>= fun e_st ->
      edges insn >>= fun es ->
      ST.return @@ List.append es e_st)

let edges_of_insns insns sub nodes proj : edges ST.t =
  let tid_list = Seq.map insns ~f:Tid_map.tid_of_elt |> Seq.to_list in
  let adjacent_tids, _ =
    List.zip_with_remainder tid_list (List.tl_exn tid_list) in
  let fallthroughs =
    List.map adjacent_tids ~f:(fun (from, t) -> (from, t, false)) in
  get_jmp_edges insns sub nodes proj >>= fun jmp_edges ->
  ST.return @@ List.append jmp_edges fallthroughs

let build sub proj : (edges * tidmap) =
  let irg = Sub.to_cfg sub in
  let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) irg in
  
  let lead_tidmap = Tid_map.t_of_sub sub in
  let init_state = init lead_tidmap in

  let builder = Seq.fold nodes ~init:(ST.return [])
                  ~f:(fun st node ->
                    let insns = insns_of_node node in
                    edges_of_insns insns sub nodes proj)
  in
  let (edges, final_state) = ST.run builder init_state in
  edges, final_state.tidmap

