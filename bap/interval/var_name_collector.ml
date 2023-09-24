open Core_kernel
open Bap.Std
open Common

let rec run (e : Bil.exp) : SS.t =
  match e with
  | Bil.Var v -> SS.singleton @@ Var.name v
  | Bil.Load (_, idx, _, _) -> run idx
  | Bil.Store (_, idx, v, _, _) ->
    SS.union (run idx) (run v)
  | Bil.BinOp (_, x, y) ->
    SS.union (run x) (run y)
  | Bil.UnOp (_, x) -> run x
  | Bil.Int _ -> SS.empty
  | Bil.Cast (cast, n, e) -> run e
  | Bil.Ite (cond, then', else') ->
    SS.union (run then') (run else')
  | Bil.Unknown (str, _) -> SS.empty
  | Bil.Let (v, exp, body) ->
    SS.union (SS.singleton (Var.name v)) (run exp)
    |> SS.union (run body)
  | Bil.Extract (hi, lo, e) -> run e
  | Bil.Concat (x, y) ->
    SS.union (run x) (run y)

let run_on_def ?(rhsonly : bool = false) d =
  let lhs = SS.singleton @@ Var.name @@ Def.lhs d in
  let rhs = run (Def.rhs d) in
  if rhsonly
  then rhs
  else SS.union lhs rhs

let run_on_defs ?(rhsonly : bool = false)
      (d : def term list) : SS.t =
  List.fold d ~init:SS.empty ~f:(fun all curdef ->
    SS.union all @@ run_on_def ~rhsonly curdef)

let run_on_jmp (j : jmp term) : SS.t =
  let cnd_vars = run (Jmp.cond j) in
  match Jmp.kind j with
  | Call c -> begin match Call.target c with
    | Indirect exp -> run exp |> SS.union cnd_vars
    | _ -> cnd_vars end
  | Goto (Indirect exp) -> run exp |> SS.union cnd_vars
  | _ -> cnd_vars

let run_on_phi ?(rhsonly : bool = false) p =
  let lhs = if rhsonly
    then SS.empty
    else SS.singleton (Var.name (Phi.lhs p)) in
  Phi.values p
  |> Seq.map ~f:(fun x -> run (snd x))
  |> Seq.fold ~init:lhs ~f:SS.union

let run_on_elt ?(rhsonly : bool = false) = function
  | `Phi p -> run_on_phi ~rhsonly p
  | `Def d -> run_on_def ~rhsonly d
  | `Jmp j -> run_on_jmp j
