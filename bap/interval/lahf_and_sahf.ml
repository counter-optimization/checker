open Core_kernel
open Bap.Std

type t = Tidset.t

let tid_part_of_transform = Tidset.mem

let num_sahf_insns = 6
let num_lahf_insns = 1
let lahf_insn_var_names = ["RAX"; "SF"; "ZF"; "AF"; "PF"; "CF"]
                          |> Common.SS.of_list

let analyze rpo bbize =
  let g = Seq.nth in
  let is_rax_load d =
    match Def.rhs d with
    | Bil.Cast (HIGH, 8, Bil.Cast (LOW, 16, Bil.Var v)) ->
      String.Caseless.equal (Var.name v) "rax"
    | _ -> false in
  let is_flag_store d =
    Set.mem Common.ABI.flag_names (Var.name (Def.lhs d)) &&
    match Def.rhs d with
    | Bil.Extract (hi, lo, subexp) when hi = lo -> true
    | _ -> false in
  let likely_sahf elts =
    match g elts 0, g elts 1 with
    | Some (`Def raxload), Some (`Def sfload) ->
      is_rax_load raxload && is_flag_store sfload
    | _ -> false in
  let likely_lahf elts =
    match Seq.hd elts with
    | Some (`Def d) ->
      if String.equal "RAX" (Var.name (Def.lhs d))
      then
        let rhs_var_names = Var_name_collector.run (Def.rhs d) in
        Common.SS.equal rhs_var_names lahf_insn_var_names
      else false
    | _ -> false in
  let rec analyze_elts elts intx ins =
    match Seq.hd elts with
    | Some e ->
      if likely_sahf elts
      then analyze_elts (Seq.drop elts num_sahf_insns) false ins
      else if likely_lahf elts
      then analyze_elts (Seq.drop elts num_lahf_insns) true ins
      else if intx
      then
        let tid = Common.elt_to_tid e in
        analyze_elts (Seq.drop elts 1) intx (Set.add ins tid)
      else analyze_elts (Seq.drop elts 1) intx ins
    | None -> (intx, ins) in
  let rec loop rpo intx ins =
    match Seq.hd rpo with
    | Some node ->
      let bb = bbize node in
      let elts = Blk.elts bb in
      let (intx, ins) = analyze_elts elts intx ins in
      let rpo' = Seq.drop rpo 1 in
      loop rpo' intx ins
    | None -> ins in
  let initial_in_transform = false in
  let initial_tids_in_transform = Tidset.empty in
  loop rpo initial_in_transform initial_tids_in_transform
