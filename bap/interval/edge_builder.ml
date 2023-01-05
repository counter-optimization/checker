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
  (* let () = printf "fromtid is %a\n%!" Tid.ppo fromtid in *)
  (* let () = printf "ret insn tid is %s\n%!" (Tid.to_string ret_insn_tid) in *)
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
     (* let () = printf "handling call\n%!" in *)
     begin
       let call_target = Call.target c in
       match call_target with
       | Direct totid ->
          begin
            (* let () = printf "direct call target is %s\n%!" (Tid.to_string totid) in *)
            let callee = sub_of_tid totid proj in
            (* let () = printf "callee tid is %s\n%!" (Term.tid callee |> Tid.to_string) in *)
            add_callee callee >>= fun () ->
            
            (* let other_tidmap = Tid_map.t_of_sub callee in *)
            (* update_tidmap other_tidmap >>= fun () -> *)

            let actual_totid = first_insn_tid_of_sub callee in
            ST.return [(fromtid, actual_totid, true)] >>= fun es ->

            let last_elt = last_insn_of_sub callee in
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
  | Int (_, _)
    | Ret _ -> ST.return []

let edges_of_jump_intraproc j sub nodes proj : edges ST.t =
  let fromtid = Term.tid j in
  (* let ret_insn_tid = get_ret_insn_tid nodes in *)
  (* let () = printf "fromtid is %a\n%!" Tid.ppo fromtid in *)
  (* let () = printf "ret insn tid is %s\n%!" (Tid.to_string ret_insn_tid) in *)
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
          (* let () = printf "proccessing call fromtid %a\n%!" Tid.ppo fromtid in *)
          let first_insn_of_ret_blk = first_insn_of_blk_tid totid sub in
          let first_insns_tid = Tid_map.tid_of_elt first_insn_of_ret_blk in
          (* let () = printf "In Call, adding ret edge: (%a, %a)\n%!" *)
          (*            Tid.ppo fromtid *)
          (*            Tid.ppo first_insns_tid *)
          (* in *)
          ST.return [(fromtid, first_insns_tid, false)]
         | _ -> ST.return []
      (* match tolabel with *)
      (* | Direct totid -> ST.return [] *)
      (* | Indirect exp -> ST.return [] *)
     end
  | Goto _
  | Int (_, _)
  | Ret _ -> ST.return []

let get_jmp_edges insns sub nodes proj =
  let edges insn =
    match insn with
    (* | `Jmp j -> edges_of_jump j sub nodes proj *)
    | `Jmp j -> edges_of_jump_intraproc j sub nodes proj
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

let get_builder_for_sub sub proj : edges ST.t =
  let irg = Sub.to_cfg sub in
  let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) irg in
  
  let lead_tidmap = Tid_map.t_of_sub sub in
  let init_state = ST.put (init lead_tidmap) >>= fun () ->
                   ST.return []
  in
  
  Seq.fold nodes ~init:init_state
    ~f:(fun st node ->
      st >>= fun prev_edges -> 
      let insns = insns_of_node node in
      edges_of_insns insns sub nodes proj >>= fun new_edges ->
      ST.return @@ List.append new_edges prev_edges)

let run (outermost : Sub.t) (proj : Project.t) : (edges * tidmap) =
  let rec loop worklist seen edges st : edges * t =
    let eff_worklist = Callees.diff worklist seen
                       |> Callees.to_list
    in
    match List.hd eff_worklist with
    | None -> (edges, st)
    | Some sub when Callees.mem seen sub ->
       let worklist' = Callees.remove worklist sub in
       loop worklist' seen edges st
    | Some sub ->
       (* let () = iter_insns sub in (\* print the callee's bir for debugging *\) *)
       
       let builder = get_builder_for_sub sub proj in
       let (callee_edges, callee_st) = ST.run builder st in
       
       let edges' = List.append callee_edges edges in
       let st' = merge st callee_st in
       
       let worklist' = Callees.union worklist st'.callees in
       let worklist' = Callees.remove worklist' sub in
       
       let seen' = Callees.add seen sub in
       
       loop worklist' seen' edges' st'
  in
  let init_seen = Callees.empty in
  let init_worklist = Callees.singleton outermost in
  let init_edges = [] in
  let init_st = empty in
  let (edges, st) = loop init_worklist init_seen init_edges init_st in
  if List.is_empty edges
  then
    begin
      let err_str = sprintf "in Edge_builder.run, didn't find edges for sub %s" (Sub.name outermost) in
      failwith err_str
    end
  else
    edges, st.tidmap

let run_one (sub : Sub.t) (proj : Project.t) : (edges * tidmap) =
  let builder = get_builder_for_sub sub proj in
  let (edges, st) = ST.run builder empty in
  edges, st.tidmap
