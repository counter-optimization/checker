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

(* need datadeps since we need to know which flags the current insn will
   set, but we are at the lifted BAP IR level, not ISA-specific assembly lvl *)
let denote_elt (datadeps : Live_variables.t) (elt : Blk.elt) (prev_env : env) =
  match elt with
  | `Def d ->
     let killed_def_var = SS.singleton @@ Var.name @@ Def.lhs d in
     let def_tid = Term.tid d in
     let killed_flags = Live_variables.get_live_flags_of_prev_def_tid datadeps ~prev_def_tid:def_tid in
     let kill = SS.union killed_flags killed_def_var in
     let gen = Var_name_collector.run @@ Def.rhs d in
     SS.diff prev_env kill
     |> SS.union gen
  | `Jmp j ->
     let gen = Var_name_collector.run @@ Jmp.cond j in
     SS.union prev_env gen
  | `Phi p ->
     let killed_def_var = SS.singleton @@ Var.name @@ Phi.lhs p in
     let phi_tid = Term.tid p in
     let killed_flags = Live_variables.get_live_flags_of_prev_def_tid datadeps ~prev_def_tid:phi_tid in
     let kill = SS.union killed_flags killed_def_var in
     let gen = Phi.values p |> phi_values_to_vars in
     SS.diff prev_env kill
     |> SS.union gen

let run_on_cfg (type g d) (module G : Graph with type t = g and type node = Calling_context.t) g tidmap datadeps : t =
  let interp_node cc =
    let tid = Calling_context.to_insn_tid cc in
    let elt = match Tid_map.find tidmap tid with
      | Some elt -> elt
      | None -> failwith @@
                  sprintf
                    "in Liveness.run_on_cfg, couldn't find tid %a in tidmap"
                    Tid.pps tid
    in
    denote_elt datadeps elt
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
  
      
