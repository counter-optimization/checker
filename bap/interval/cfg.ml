open Core_kernel
open Bap.Std
open Graphlib.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

module Cfg = Bap.Std.Graphs.Cfg
module IrCfg = Bap.Std.Graphs.Ir

module Names = Var_name_scraper.VarName

let target_func = "Hacl_Impl_Curve25519_Field51_fmul"

let print_if_target_func (name, block, cfg) =
  if String.(name = target_func)
  then
    begin
      let sub = Sub.lift block cfg in
      let irg = Sub.to_cfg sub in
      
      Format.printf "Vars are:\n%!";
      let vars = Var_name_scraper.get_all_vars irg in
      Names.Set.iter vars ~f:(fun name -> Format.printf "%s\n%!" name);

      let final_sol = Interval_analysis.run sub in
      let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) irg in
      let first_node = Seq.hd_exn nodes in
      let first_bb_sol = Solution.get final_sol first_node in
      Format.printf
        "first_bb_sol: %a\n%!"
        Sexp.pp
        (Map_lattice.Interval.M.sexp_of_t Interval.sexp_of_t first_bb_sol);


      let num_nodes_in_graph = Seq.length nodes in
      Format.printf "There were %d nodes in the irg\n%!" num_nodes_in_graph;

      (* Seq.iter nodes ~f:(fun node -> *)
      (*     let blk = Graphs.Ir.Node.label node in *)
      (*     let blk_id = Term.tid blk in  *)
      (*     Format.printf "Block (%a):\n%a\n\n%!" Tid.pp blk_id Blk.pp blk) *)
    end
  else
    ()

let pass proj =
  Project.symbols proj |>
  Symtab.to_sequence |>
    Seq.iter ~f:print_if_target_func
        
let () =
  Bap_main.Extension.declare @@ fun _ctxt ->
                                Project.register_pass' pass;
                                Ok ()
