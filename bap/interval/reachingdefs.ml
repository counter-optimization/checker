open Core_kernel
open Bap.Std
open Graphlib.Std

type varname = string [@@deriving compare, sexp]

module Def = struct
  type t = tid option * varname [@@deriving compare, sexp]
end

module Cmp = struct
  include Def
  include Comparator.Make(Def)
end

module DefSet = Set.Make_using_comparator(Cmp)

type def = Def.t

type defset = DefSet.t

type sol = (Calling_context.t, defset) Solution.t

let kill defset var =
  let is_def_of_var (mtid, varname) = String.(varname = var) in
  let keep d = not (is_def_of_var d) in
  DefSet.filter defset ~f:keep

let gen var tid defset =
  DefSet.add defset (Some tid, var)

let denote_elt elt defs =
  match elt with
  | `Def d ->
     let varname = Var.name (Bap.Std.Def.lhs d) in
     let tid = Term.tid d in
     kill defs varname
     |> gen varname tid
  | `Phi p ->
     let varname = Var.name (Phi.lhs p) in
     let tid = Term.tid p in
     kill defs varname
     |> gen varname tid
  | `Jmp j -> defs
  

let init_sol (type g d) (module G : Graph with type t = g and type node = Calling_context.t) sub cfg_firstnode =
  let default_of_var var = (None, Var.name var) in
  let init_defs = Sub.free_vars sub
                  |> Set.to_list
                  |> List.map ~f:default_of_var
                  |> DefSet.of_list in
  let default_sol = DefSet.empty in
  let sol_map = G.Node.Map.set G.Node.Map.empty
                     ~key:cfg_firstnode
                     ~data:init_defs in
  Solution.create sol_map default_sol

let run_on_cfg (type g) (module G : Graph with type t = g and type node = Calling_context.t) g sub tidmap cfg_firstnode =
  let interp_node cc =
    let tid = Calling_context.to_insn_tid cc in
    let elt = match Tid_map.find tidmap tid with
      | Some elt -> elt
      | None -> failwith @@
                  sprintf
                    "in Reachingdefs.run_on_cfg, couldn't find tid %a in tidmap"
                    Tid.pps tid in
    denote_elt elt
  in
  let merge = DefSet.union in
  let equal = DefSet.equal in
  let widen_with_step _nvisits _node = DefSet.union in
  let init_sol = init_sol (module G) sub cfg_firstnode in
  Graphlib.fixpoint
    (module G)
    g
    ~step:widen_with_step
    ~init:init_sol
    ~equal
    ~merge
    ~f:interp_node  
