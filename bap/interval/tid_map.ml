open Core
open Bap.Std

module M = Map.Make_binable_using_comparator(Tid)
include M

let tid_of_elt = function
  | `Def x -> Term.tid x
  | `Phi x -> Term.tid x
  | `Jmp x -> Term.tid x

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
