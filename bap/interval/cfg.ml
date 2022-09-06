open Core_kernel
open Bap.Std
open Graphlib.Std

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
        (Map_lattice.Interval.M.sexp_of_t Interval.sexp_of_t iml)

let print_sol sol : unit =
  Solution.enum sol |>
    Sequence.iter ~f:(fun (n, iml) ->
        Format.printf "Node (%a): \n%!" Graphs.Ir.Node.pp n;
        print_iml iml)

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
      Format.printf "Final sol is: \n%!";
      print_sol final_sol
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
