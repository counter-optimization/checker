open Core
open Bap.Std
open Graphlib.Std
open Common

module Env = struct
  type t = SS.t

  let empty = SS.empty

  let merge = SS.union

  let equal = SS.equal

  let widen_with_step _numiter _node old_st new_st = merge old_st new_st
end

type env = Env.t

type t = (Calling_context.t, Env.t) Solution.t

let live_at_tid (tid : tid) (sol : t) : Env.t =
  let cc = Calling_context.of_tid tid in
  Solution.get sol cc

let phi_values_to_vars vals =
  Seq.fold vals ~init:SS.empty ~f:(fun all_vars (_, exp) ->
      SS.union all_vars @@ Var_name_collector.run exp)

let denote_elt (elt : Blk.elt) (prev_env : env) =
  match elt with
  | `Def d ->
     let kill = SS.singleton @@ Var.name @@ Def.lhs d in
     let gen = Var_name_collector.run @@ Def.rhs d in
     SS.diff prev_env kill
     |> SS.union gen
  | `Jmp j ->
     let gen = Var_name_collector.run @@ Jmp.cond j in
     SS.union prev_env gen
  | `Phi p ->
     let kill = SS.singleton @@ Var.name @@ Phi.lhs p in
     let gen = Phi.values p |> phi_values_to_vars in
     SS.diff prev_env kill
     |> SS.union gen

let run_on_cfg (type g d) (module G : Graph with type t = g and type node = Calling_context.t) g tidmap : t =
  let interp_node cc =
    let tid = Calling_context.to_insn_tid cc in
    let elt = match Tid_map.find tidmap tid with
      | Some elt -> elt
      | None -> failwith @@
                  sprintf
                    "in Liveness.run_on_cfg, couldn't find tid %a in tidmap"
                    Tid.pps tid
    in
    denote_elt elt
  in
  let init_sol = Solution.create G.Node.Map.empty Env.empty in
  Graphlib.fixpoint
    (module G)
    g
    ~rev:true
    ~step:Env.widen_with_step
    ~init:init_sol
    ~equal:Env.equal
    ~merge:Env.merge
    ~f:interp_node
  
      
