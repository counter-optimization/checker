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

      let init_sol = Interval_analysis.make_initial_solution sub in
      let first_blk = Option.value_exn (Term.first blk_t sub) in
      let sol_for_first_blk = Solution.get init_sol first_blk in
      Format.printf "sol_for_first_blk: %a\n%!"
        Sexp.pp
        (Map_lattice.Interval.M.sexp_of_t Interval.sexp_of_t sol_for_first_blk)
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
