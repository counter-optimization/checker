open Core_kernel
open Bap.Std
open Graphlib.Std

module M = Map.Make_binable_using_comparator(Tid)
include M

let tid_of_elt = function
  | `Def x -> Term.tid x
  | `Phi x -> Term.tid x
  | `Jmp x -> Term.tid x

let elt_same (e1 : Blk.elt) (e2 : Blk.elt) : bool =
  match e1, e2 with
  | `Def d1, `Def d2 -> Term.same d1 d2
  | `Jmp j1, `Jmp j2 -> Term.same j1 j2
  | `Phi p1, `Phi p2 -> Term.same p1 p2
  | _, _ -> false

let add_insns insns tidmap =
  Seq.fold insns
    ~init:tidmap
    ~f:(fun tidmap insn ->
      set tidmap ~key:(tid_of_elt insn) ~data:insn)

let t_of_sub_nodes nodes insns_of_node =
  Seq.fold nodes
    ~init:empty
    ~f:(fun tmap n ->
      let insns = insns_of_node n in
      add_insns insns tmap)

let t_of_sub sub =
  let irg = Sub.to_cfg sub in
  let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) irg in
  let insns_of_node = fun n -> Blk.elts @@ Graphs.Ir.Node.label n in
  t_of_sub_nodes nodes insns_of_node

let merge tm1 tm2 =
  fold tm2 ~init:tm1
    ~f:(fun ~key ~(data : Blk.elt) cur ->
      if mem cur key && not (elt_same data (find_exn cur key))
      then
        begin
          let tid_s = Tid.to_string key in
          failwith @@ sprintf "tid %s already present in tidmap with different insns" tid_s
        end
      else
        set cur ~key ~data)
