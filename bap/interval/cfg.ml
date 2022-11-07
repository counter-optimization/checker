open Core
open Bap.Std
open Graphlib.Std
open Common

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

module Cfg = Bap.Std.Graphs.Cfg
module IrCfg = Bap.Std.Graphs.Ir

module Names = Var_name_scraper.VarName

let print_iml iml : unit =
  Format.printf
        "%a\n%!"
        Sexp.pp
        (Map_lattice.Interval.M.sexp_of_t Wrapping_interval.sexp_of_t iml)

let print_sol sol : unit =
  Solution.enum sol |>
    Sequence.iter ~f:(fun (n, iml) ->
        Format.printf "Node (%a): \n%!" Graphs.Ir.Node.pp n;
        print_iml iml)

let insns_of_node n = Blk.elts @@ Graphs.Ir.Node.label n
let first_insn_of_blk b =
  let insns = Blk.elts b in
  match Seq.hd insns with
  | Some i -> i
  | None -> failwith "In first_insn_of_blk, couldn't find first insn"

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

let sub_to_insn_graph sub =
  let irg = Sub.to_cfg sub in
  let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) irg in

  let print_elt = function
      | `Def d -> Format.printf "Def: %s\n%!" @@ Def.to_string d
      | `Phi p -> Format.printf "Phi: %s\n%!" @@ Phi.to_string p
      | `Jmp j -> Format.printf "Jmp: %s\n%!" @@ Jmp.to_string j
  in

  (* Create the tidmap *)
  let tidmap = Tid_map.t_of_sub_nodes nodes insns_of_node in

  (* building graph *)
  let ret_insn_tid = get_ret_insn_tid nodes in
  let intraproc_edge_of_jump j =
    let fromtid = Term.tid j in
    match Jmp.kind j with
    | Goto (Direct totid) ->
       (* totid is the tid of a blk, but we need the first insn of that blk *)
       let the_blk = Term.find_exn blk_t sub totid in
       let first_insn = first_insn_of_blk the_blk in
       Some (fromtid, Tid_map.tid_of_elt first_insn, ())
    | Goto _ when Tid.equal fromtid ret_insn_tid -> None
    | Goto (Indirect _) ->
       failwith "Indirect jumps not handled in edge building yet"
    | Int (_, _)
      | Call _
      | Ret _ -> None
  in
  let get_jmp_edges insns =
    let edge insn =
      match insn with
      | `Jmp j -> intraproc_edge_of_jump j
      | _ -> None
    in
    Seq.fold insns ~init:[] ~f:(fun edges insn ->
        match edge insn with
        | Some e -> List.cons e edges
        | _ -> edges)
  in
  let edges_of_insns insns =
    let tid_list = Seq.map insns ~f:Tid_map.tid_of_elt |> Seq.to_list in
    let adjacent_tids, _ =
      List.zip_with_remainder tid_list (List.tl_exn tid_list) in
    let fallthroughs =
      List.map adjacent_tids ~f:(fun (from, t) -> (from, t, ())) in
    let jmp_edges = get_jmp_edges insns in
    List.append fallthroughs jmp_edges
  in
  let edges = Seq.fold nodes ~init:[] ~f:(fun edges n ->
                  let insns = insns_of_node n in
                  List.append (edges_of_insns insns) edges)
  in
  
  (* CFG *)
  let module G = Graphlib.Make(Tid)(Unit) in
  let cfg = Graphlib.create (module G) ~edges () in

  (* AbsInt *)
  let module ProdIntvlxTaint = DomainProduct(Wrapping_interval)(Checker_taint.Analysis) in
  let module E = NumericEnv(ProdIntvlxTaint) in
  let module AbsInt = AbstractInterpreter(ProdIntvlxTaint) in
  let final_sol = Graphlib.fixpoint
                    (module G)
                    cfg 
                    ~step:E.widen_with_step
                    ~init:(E.initial_solution sub cfg) 
                    ~equal:E.equal
                    ~merge:E.merge
                    ~f:(fun tid ->
                      let elt = Tid_map.find_exn tidmap tid in
                      AbsInt.denote_elt elt)
  in
  let print_sol sol =
    Solution.enum final_sol |>
      Sequence.iter ~f:(fun (tid, env) ->
          Format.printf "TID(%s) -> %!" (Tid.to_string tid);
          E.pp env)
  in
  let () = print_sol final_sol in

  let module CompSimpChecker = Comp_simp.Checker(Wrapping_interval) in
  let comp_simp_checker_res =
    List.fold edges
      ~init:CompSimpChecker.empty
      ~f:(fun acc_res (from', to', ()) ->
        let prod_from_state = Solution.get final_sol from' in
        let intvl_from_state = E.select_from_prod_env prod_from_state ProdIntvlxTaint.first in
        (* let taint_from_state = E.select_from_prod_env prod_from_state ProdIntvlxTaint.second in *)
        let elt = Tid_map.find_exn tidmap to' in
        let check_res = CompSimpChecker.check_elt elt intvl_from_state in
        CompSimpChecker.join acc_res check_res)
  in
  let () = Format.printf "Comp simp checker results are:\n%!" in
  let () = CompSimpChecker.print_results comp_simp_checker_res in

  List.iter edges ~f:(fun (f, t, ()) ->
      let from_str = Tid.to_string f in
      let to_str = Tid.to_string t in
      Format.printf "(%s,%s)\n%!" from_str to_str);
      
  Tid_map.iteri tidmap
    ~f:(fun ~key ~data ->
      Format.printf "TID(%s) :: %!" (Tid.to_string key);
      print_elt data)

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

let check_fn (name, block, cfg) =
  let sub = Sub.lift block cfg in
  let () = iter_insns sub in
  (* Run the checkers *)
  sub_to_insn_graph sub
