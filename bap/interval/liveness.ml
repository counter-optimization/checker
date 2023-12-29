open Core_kernel
open Bap.Std
open Graphlib.Std
open Common

module Env = struct
  type t = String.Set.t [@@deriving sexp, compare, equal]

  let empty = String.Set.empty

  let merge = String.Set.union

  let equal = String.Set.equal

  let widen_with_step _numiter _node old_st new_st =
    merge old_st new_st
end

type env = Env.t [@@deriving sexp,compare,equal]

type t = (Tid.t, Env.t) Solution.t

let sexp_of_t sol = Solution.enum sol
                    |> Seq.map ~f:(fun (cc, env) ->
                      Sexp.List [Tid.sexp_of_t cc;
                                 Env.sexp_of_t env])
                    |> Seq.to_list
                    |> fun x -> Sexp.List x

let equal = Solution.equal ~equal:Env.equal

let enum = Solution.enum

let liveness_at_tid : t -> tid -> Env.t = Solution.get 

let var_live_at_tid sol tid varname =
  let liveness = liveness_at_tid sol tid in
  String.Set.mem liveness varname

let phi_values_to_vars vals =
  Seq.fold vals ~init:String.Set.empty ~f:(fun all_vars (_, exp) ->
    String.Set.union all_vars @@ Var_name_collector.run exp)

let assn_of_def (dt : def term) : string =
  Var.name @@ Def.lhs dt

let get_dead_defs ?(flagsonly : bool = true)
      (sol : t) (tidmap : Blk.elt Tid.Map.t)
  : Tidset.t =
  let is_flag = Common.AMD64SystemVABI.var_name_is_flag in
  let assn_of_elt = function
    | `Def d -> Some (Var.name (Def.lhs d))
    | _ -> None
  in
  let get_info tid lives =
    match Tid_map.find tidmap tid with
    | None -> (true, false)
    | Some elt ->
      begin match assn_of_elt elt with
      | Some v -> (String.Set.mem lives v, is_flag v)
      | None -> (true, false)
      end
  in
  enum sol
  |> Seq.fold ~init:Tidset.empty ~f:(fun dead (tid, lives) ->
    let (is_live, is_flag) = get_info tid lives in
    let is_dead = not is_live in
    if is_dead && ((not flagsonly) || is_flag)
    then Tidset.add dead tid
    else dead)


let denote_elt (elt : Blk.elt) (prev_env : env) =
  match elt with
  | `Def d ->
    let kill = String.Set.singleton @@ assn_of_def d in
    let gen = Var_name_collector.run @@ Def.rhs d in
    String.Set.diff prev_env kill
    |> String.Set.union gen
  | `Jmp j ->
    let gen = Var_name_collector.run @@ Jmp.cond j in
    String.Set.union prev_env gen
  | `Phi p ->
    let kill = String.Set.singleton @@ Var.name @@ Phi.lhs p in
    let gen = Phi.values p |> phi_values_to_vars in
    String.Set.diff prev_env kill
    |> String.Set.union gen

let run_on_cfg (type g d) (module G : Graph with type t = g and type node = Tid.t) g tidmap : t =
  let interp_node tid =
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
