open Core_kernel
open Bap.Std

module SS = Set.Make_binable_using_comparator(String)

type t = SS.t

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
