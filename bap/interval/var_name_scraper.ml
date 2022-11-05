open Core
open Bap.Std
open Graphlib.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge
module IrCfg = Bap.Std.Graphs.Ir

let package = "uarch-checker"

module VarName = struct
  module Set = Set.Make_binable_using_comparator(String)

  module Domain = struct
    let order set1 set2 : KB.Order.partial =
      if Set.equal set1 set2
      then KB.Order.EQ
      else
        if Set.is_subset set1 ~of_:set2
        then KB.Order.LT
        else
          if Set.is_subset set2 ~of_:set1
          then KB.Order.GT
          else KB.Order.NC

    let join s1 s2 = Ok (Set.union s1 s2)
    let inspect = Set.sexp_of_t
    let empty = Set.empty
  end
end

let var_name_domain =
  KB.Domain.define
    "var-name-set"
    ~empty:VarName.Domain.empty
    ~join:VarName.Domain.join
    ~order:VarName.Domain.order
    ~inspect:VarName.Domain.inspect

let slot : (Analysis_info.t, VarName.Set.t) KB.slot =
  KB.Class.property
    Analysis_info.cls
    "var-names"
    var_name_domain

let (%>) f g = fun x -> g (f x)

let var_name_of_phi = Phi.var %> T.Var.name

let var_name_of_def = Def.var %> T.Var.name

let ns = Var.name %> VarName.Set.singleton

let elt_to_vars (elt : Blk.elt) : VarName.Set.t =
    match elt with
    | `Phi p -> VarName.Set.singleton @@ var_name_of_phi p
    | `Def d -> VarName.Set.singleton @@ var_name_of_def d
    | `Jmp j -> VarName.Set.empty

(* let elt_to_tids (elt : Blk.elt) : Tid.Set.t = *)
(*   match elt with *)
(*   | `Phi p -> Phi.lhs p |> Term.tid |> Tid.Set.singleton *)
(*   | `Def d -> Def.lhs d |> Term.tid |> Tid.Set.singleton *)
(*   | `Jmp j -> Tid.Set.empty (\* this might be wrong *\) *)

let block_to_vars (b : blk term) : VarName.Set.t =
  let elts = Blk.elts b in
  let frees = Blk.free_vars b in
  let free_names = Var.Set.fold frees
                     ~init:(VarName.Set.empty)
                     ~f:(fun acc v ->
                       VarName.Set.union acc @@ ns v) in
  Seq.fold elts ~init:free_names ~f:(fun acc elt ->
      VarName.Set.union acc @@ elt_to_vars elt)

let get_all_vars (irg : Graphs.Ir.t) : VarName.Set.t =
  let nodes = Graphlib.reverse_postorder_traverse (module IrCfg) irg in
  Seq.fold nodes
    ~init:(VarName.Set.empty)
    ~f:(fun acc node ->
      let label = IrCfg.Node.label node in
      VarName.Set.union acc @@ block_to_vars label)

(* let promise_result (fn_name : string) (irg : Graphs.Ir.t) : unit = *)
  
