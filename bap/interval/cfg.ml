open Core_kernel
open Bap.Std
open Graphlib.Std
open Common

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

module Cfg = Bap.Std.Graphs.Cfg
module IrCfg = Bap.Std.Graphs.Ir

module Names = Var_name_scraper.VarName

let target_func = "ite_xorident"

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

let sub_to_insn_graph sub =
  let irg = Sub.to_cfg sub in
  let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) irg in

  let insns_of_node node =
    Graphs.Ir.Node.label node |> Blk.elts
  in

  (* Create the tidmap *)
  let module TidMap = Map.Make_binable_using_comparator(Tid) in
  let tid_of_elt = function
      | `Def d -> Term.tid d
      | `Phi p -> Term.tid p
      | `Jmp j -> Term.tid j
  in
  let print_elt = function
      | `Def d -> Format.printf "Def: %s\n%!" @@ Def.to_string d
      | `Phi p -> Format.printf "Phi: %s\n%!" @@ Phi.to_string p
      | `Jmp j -> Format.printf "Jmp: %s\n%!" @@ Jmp.to_string j
  in
  let add_insns_to_tid_map insns tidmap =
    Seq.fold insns
      ~init:tidmap
      ~f:(fun tidmap insn ->
        TidMap.set tidmap ~key:(tid_of_elt insn) ~data:insn)
  in
  let tidmap = Seq.fold nodes ~init:TidMap.empty
                 ~f:(fun tidmap node ->
                   let insns = insns_of_node node in
                   add_insns_to_tid_map insns tidmap)
  in

  (* building graph *)
  let edge_of_jump j =
    let fromtid = Term.tid j in
    match Jmp.kind j with
    | Call _ -> failwith "call not supported for edge building yet"
    | Goto (Direct totid) ->
    (* totid is the tid of a blk, but we need the first insn of that blk *)
       let the_blk = Term.find_exn blk_t sub totid in
       let first_insn = Blk.elts the_blk |> Seq.hd_exn in
       Some (fromtid, tid_of_elt first_insn, ())
    | Goto (Indirect _) ->
       Format.printf "TODO: handle indirect jumps in edge building for absint cfg\n%!";
       None
    | Ret _ -> failwith "ret not supported for edge building yet"
    | Int (_, _) -> None
  in
  let get_jmp_edges insns =
    let edge insn =
      match insn with
      | `Jmp j -> edge_of_jump j
      | _ -> None
    in
    Seq.fold insns ~init:[] ~f:(fun edges insn ->
        match edge insn with
        | Some e -> List.cons e edges
        | _ -> edges)
  in
  let edges_of_insns insns =
    let tid_list = Seq.map insns ~f:tid_of_elt |> Seq.to_list in
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
                      let elt = TidMap.find_exn tidmap tid in
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
      ~init:(CompSimpChecker.init_results)
      ~f:(fun acc_res (from', to', ()) ->
        let prod_from_state = Solution.get final_sol from' in
        let intvl_from_state = E.select_from_prod_env prod_from_state ProdIntvlxTaint.first in
        let elt = TidMap.find_exn tidmap to' in
        let check_res = CompSimpChecker.check_elt elt intvl_from_state in
        CompSimpChecker.join_results acc_res check_res)
  in
  let () = CompSimpChecker.print_results comp_simp_checker_res in

  List.iter edges ~f:(fun (f, t, ()) ->
      let from_str = Tid.to_string f in
      let to_str = Tid.to_string t in
      Format.printf "(%s,%s)\n%!" from_str to_str);
      
  TidMap.iteri tidmap
    ~f:(fun ~key ~data ->
      Format.printf "TID(%s) :: %!" (Tid.to_string key);
      print_elt data)

let iter_insns sub : unit =
  let irg = Sub.to_cfg sub in
  let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) irg in
  
  let print_sub_defs graphnode =
    let bb = Graphs.Ir.Node.label graphnode in
    let insns = Blk.elts bb in
    let print_insn = function
      | `Def d -> Format.printf "Def: %s\n%!" @@ Def.to_string d
      | `Phi p -> Format.printf "Phi: %s\n%!" @@ Phi.to_string p
      | `Jmp j -> Format.printf "Jmp: %s\n%!" @@ Jmp.to_string j
    in
    Seq.iter insns ~f:print_insn
  in
  
  let () = Format.printf "nodes are:\n%!" in
  Seq.iter nodes ~f:print_sub_defs

let check_fn (name, block, cfg) =
  let sub = Sub.lift block cfg in
  (* Print the SSA *)
  let sub_ssa = Sub.ssa sub in
  let () = iter_insns sub_ssa in
  (* Run the checkers *)
  let irg = Sub.to_cfg sub in
  sub_to_insn_graph sub
