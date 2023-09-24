open Core_kernel
open Bap.Std
open Graphlib.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge
module Out_channel = Stdlib.Out_channel

let outfile = "prog-conds.txt"

let rec contains_ite (exp : Bil.exp) : bool =
  match exp with
  | Bil.Load (_, idx, _, _) ->
    contains_ite idx
  | Bil.Store (_, idx, data, _, _) ->
    contains_ite idx ||
    contains_ite data
  | Bil.BinOp (_, left, right) ->
    contains_ite left ||
    contains_ite right
  | Bil.UnOp (_, subexp) ->
    contains_ite subexp
  | Bil.Var _ -> false
  | Bil.Int _ -> false
  | Bil.Cast (_, _, subexp) ->
    contains_ite subexp
  | Bil.Let (_, bind, body) ->
    contains_ite bind || contains_ite body
  | Bil.Unknown (_, _) -> false
  | Bil.Ite (_, _, _) -> true
  | Bil.Extract (_, _, subexp) ->
    contains_ite subexp
  | Bil.Concat (left, right) ->
    contains_ite left || contains_ite right

let print_if_ite out_ch subname : Blk.elt -> unit = function
  | `Def d ->
    let rhs = Def.rhs d in
    let rhs_contains_ite = contains_ite rhs in
    if rhs_contains_ite
    then
      let cnd_as_str = Sexp.to_string_hum @@ Bil.sexp_of_exp rhs in
      Format.sprintf "CONDITIONAL (ITE, %s): %s\n" subname cnd_as_str
      |> Out_channel.output_string out_ch
    else
      ()
  | _ -> ()

let print_if_jmp_cnd out_ch subname : Blk.elt -> unit = function
  | `Jmp j ->
    let cnd = Jmp.cond j in
    let cnd_as_str = Sexp.to_string_hum @@ Bil.sexp_of_exp cnd in
    Format.sprintf "CONDITIONAL (JMPCC, %s): %s\n" subname cnd_as_str
    |> Out_channel.output_string out_ch
  | _ -> ()

let print_elt out_ch subname elt =
  print_if_jmp_cnd out_ch subname elt;
  print_if_ite out_ch subname elt

(* let print_if_flags out_ch = function *)
(*   | `Def d -> *)
(*      let flags = Flag_ownership.get_flag_own_tids d in *)
(*      if 0 < List.length flags *)
(*      then *)
(*        let s = Format.sprintf "flags for def term %s:\n" @@ Def.to_string d in *)
(*        Out_channel.output_string out_ch s; *)
(*        List.iter flags ~f:(fun flag -> *)
(*        let flag_s = Format.sprintf "\t%a\n" Tid.pps flag in *)
(*        Out_channel.output_string out_ch flag_s) *)
(*      else *)
(*        () *)
(*   | _ -> () *)

let print_conds_of_sub sub =
  let subname = Sub.name sub in
  let cfg = Sub.to_cfg sub in
  let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) cfg in
  let file_flags = [Out_channel.Open_append; Out_channel.Open_text] in
  Out_channel.with_open_gen file_flags 0o600 outfile @@ fun out_ch ->
  Seq.iter nodes ~f:(fun node ->
    let blk = Graphs.Ir.Node.label node in                                          
    let elts = Blk.elts blk in
    Seq.iter elts ~f:(fun elt ->
      (* print_if_flags out_ch elt; *)
      print_elt out_ch subname elt))
