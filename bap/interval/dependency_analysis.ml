open Core
open Bap.Std

type tidset = Set.M(Tid).t

module Varmap = Map.Make_binable_using_comparator(String)

module T = struct
  type t = {
      last_defd_env: tidset Varmap.t;
      tid_uses: tidset Tid_map.t;
      tid_users: tidset Tid_map.t
    }

  let empty = {
      last_defd_env = Varmap.empty;
      tid_uses = Tid_map.empty;
      tid_users = Tid_map.empty;
    }

  let rec users_transitive_closure tid st : tidset =
    match Tid_map.find st.tid_users tid with
    | None -> Set.empty (module Tid)
    | Some tidset ->
       Set.fold tidset ~init:tidset ~f:(fun alltids nexttid ->
           Set.union alltids @@ users_transitive_closure nexttid st)

  let equal x y =
    let tidset_equal = Set.equal in
    Varmap.equal tidset_equal x.last_defd_env y.last_defd_env &&
      Tid_map.equal tidset_equal x.tid_uses y.tid_uses &&
        Tid_map.equal tidset_equal x.tid_users y.tid_users

  let add_to_users ~(used : tid) ~(user : tid)
        (users : tidset Tid_map.t) : tidset Tid_map.t =
    let users' = match Tid_map.find users used with
      | Some prev_users ->
         Set.add prev_users user
      | None ->
         Set.singleton (module Tid) user
    in
    Tid_map.set users ~key:used ~data:users'

  let merge x y =
    let last_defd_env = Varmap.merge_skewed
                          x.last_defd_env
                          y.last_defd_env
                          ~combine:(fun ~key -> Set.union)
    in
    let tid_uses = Tid_map.merge_skewed
                     x.tid_uses
                     y.tid_uses
                     ~combine:(fun ~key -> Set.union)
    in
    let tid_users = Tid_map.merge_skewed
                      x.tid_users
                      y.tid_users
                      ~combine:(fun ~key -> Set.union)
    in
    { last_defd_env; tid_uses; tid_users }

  (* no infinite ascending chains, so no widening here *)
  let step numrepeats node data1 data2 =
    merge data1 data2
end

include T

let emp_tidset = Set.empty (module Tid)

let rec uses_of_exp (e : Bil.exp) (st : t) : tidset =
  match e with
  | Bil.Load (_, offset_exp, _, _) ->
     uses_of_exp offset_exp st
  | Bil.Store (_, offset_exp, store_data, _, _) ->
     uses_of_exp offset_exp st
     |> Set.union @@ uses_of_exp store_data st
  | Bil.BinOp (_, l, r) ->
     uses_of_exp l st
     |> Set.union @@ uses_of_exp r st 
  | Bil.UnOp (_, l) ->
     uses_of_exp l st
  | Bil.Var x ->
     let name = Var.name x in
     (match Varmap.find st.last_defd_env name with
      | Some defining_tid -> defining_tid
      | None -> emp_tidset)
  | Bil.Int _ ->
     emp_tidset
  | Bil.Cast (_, _, exp) ->
     uses_of_exp exp st
  | Bil.Let (_, cond, body) ->
     uses_of_exp cond st
     |> Set.union @@ uses_of_exp body st
  | Bil.Unknown (_, _) ->
     emp_tidset
  | Bil.Ite (i, t, e) ->
     uses_of_exp i st
     |> Set.union @@ uses_of_exp t st
     |> Set.union @@ uses_of_exp e st
  | Bil.Extract (_, _, exp) ->
     uses_of_exp exp st
  | Bil.Concat (l, r) ->
     uses_of_exp l st
     |> Set.union @@ uses_of_exp r st 

let denote_jmp (j : jmp term) (st : t) : t =
  let outer_tid = Term.tid j in
  let uses = match Jmp.kind j with
    | Call callee ->
       let target = Call.target callee in
       (match target with
        | Direct _totid -> emp_tidset
        | Indirect exp ->
           uses_of_exp exp st)
    | Goto (Direct _totid) -> emp_tidset
    | Goto (Indirect exp) ->
       uses_of_exp exp st
    | Ret r -> emp_tidset
    | Int (_, _) -> emp_tidset
  in
  let uses = Set.union uses @@ uses_of_exp (Jmp.cond j) st in
  let users = Set.fold uses ~init:st.tid_users ~f:(fun users used ->
                  add_to_users ~used ~user:outer_tid users)
  in
  {
    st with
    tid_uses = Tid_map.set st.tid_uses ~key:outer_tid ~data:uses;
    tid_users = users
  }
    
let denote_def (d : def term) (st : t) : t =
  let outer_tid = Term.tid d in
  let outer_tidset = Set.singleton (module Tid) outer_tid in
  let defd_var = Var.name @@ Def.lhs d in
  let uses = uses_of_exp (Def.rhs d) st in
  let users = Set.fold uses ~init:st.tid_users ~f:(fun users used ->
                  add_to_users ~used ~user:outer_tid users)
  in
  {
    last_defd_env = Varmap.set st.last_defd_env ~key:defd_var ~data:outer_tidset;
    tid_uses = Tid_map.set st.tid_uses ~key:outer_tid ~data:uses;
    tid_users = users
  }

let denote_elt (e : Blk.elt) (st : t) : t =
  match e with
  | `Def d -> denote_def d st
  | `Jmp j -> denote_jmp j st
  | `Phi p ->
     failwith "Phi should not occur in arg to Dependency_analysis.denote_elt"
