open Core_kernel
open Bap.Std
open Graphlib.Std
open Common

module Env = struct
  type t = SS.t

  let empty = SS.empty

  let merge = SS.union

  let equal = SS.equal

  let widen_with_step _numiter _node old_st new_st =
    merge old_st new_st
end

type env = Env.t

type t = (Calling_context.t, Env.t) Solution.t

let enum = Solution.enum

let liveness_at_tid (sol : t) (tid : tid) : Env.t =
  let cc = Calling_context.of_tid tid in
  Solution.get sol cc

let var_live_at_tid sol tid varname =
  let liveness = liveness_at_tid sol tid in
  SS.mem liveness varname
  
let phi_values_to_vars vals =
  Seq.fold vals ~init:SS.empty ~f:(fun all_vars (_, exp) ->
      SS.union all_vars @@ Var_name_collector.run exp)

let assn_of_def (dt : def term) : string =
  Var.name @@ Def.lhs dt

let get_dead_defs ?(flagsonly : bool = true)
      (sol : t) (tidmap : Edge_builder.tidmap)
    : Tidset.t =
  let t = Calling_context.to_insn_tid in
  let is_flag = Common.AMD64SystemVABI.var_name_is_flag in
  let assn_of_elt = function
    | `Def d -> Some (Var.name (Def.lhs d))
    | _ -> None
  in
  let get_info cc lives =
    let tid = t cc in
    match Tid_map.find tidmap tid with
    | None -> (true, false)
    | Some elt ->
       begin match assn_of_elt elt with
       | Some v -> (SS.mem lives v, is_flag v)
       | None -> (true, false)
       end
  in
  enum sol
  |> Seq.fold ~init:Tidset.empty ~f:(fun dead (cc, lives) ->
         let (is_live, is_flag) = get_info cc lives in
         let is_dead = not is_live in
         if is_dead && ((not flagsonly) || is_flag)
         then Tidset.add dead (t cc)
         else dead)
         

let denote_elt (elt : Blk.elt) (prev_env : env) =
  match elt with
  | `Def d ->
     let kill = SS.singleton @@ assn_of_def d in
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
                    Tid.pps tid in
    denote_elt elt in
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
