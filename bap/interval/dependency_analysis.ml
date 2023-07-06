open Core
open Bap.Std

type tidset = Tidset.t

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

  let users_transitive_closure tid st =
    (* this overflows stack if not tail recursive *)
    let default = Tidset.empty in
    let rec users_transitive_closure_loop
              ?(users : tidset = default)
              ~(worklist : tidset)
              st : tidset =
      if Set.is_empty worklist
      then
        users
      else
        let first = Seq.hd_exn @@ Set.to_sequence worklist in
        if Set.mem users first
        then
          let worklist = Set.remove worklist first in
          users_transitive_closure_loop ~worklist ~users st
        else
          let users = if Tid.equal tid first
                      then users
                      else Set.add users first
          in
          match Tid_map.find st.tid_users first with
          | None ->
             let worklist = Set.remove worklist first in
             users_transitive_closure_loop ~worklist ~users st
          | Some tids_to_process ->
             let worklist = Set.union worklist tids_to_process in
             let worklist = Set.remove worklist first in
             users_transitive_closure_loop ~worklist ~users st
    in
    let target_tidset = Tidset.singleton tid in
    users_transitive_closure_loop ~worklist:target_tidset st

  let has_user tid st : bool =
    let users = users_transitive_closure tid st in
    not @@ Tidset.is_empty users

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
         Tidset.singleton user
    in
    Tid_map.set users ~key:used ~data:users'

  let add_to_uses ~user ~used uses : tidset Tid_map.t =
    let uses' = match Tid_map.find uses user with
      | Some prev_used ->
         Set.add prev_used used
      | None ->
         Tidset.singleton used
    in
    Tid_map.set uses ~key:user ~data:uses'

  let simultaneous_add ~user ~used st =
    { st with
      tid_uses = add_to_uses ~user ~used st.tid_uses;
      tid_users = add_to_users ~user ~used st.tid_users }

  let add_flag_ownership_dependencies flagownership
        (ccs : Calling_context.t Seq.t)
        st : t =
    Seq.fold ccs ~init:st ~f:(fun st cc ->
        let tid = Calling_context.to_insn_tid cc in
        match Tid_map.find flagownership tid with
        | None -> st
        | Some flagtids ->
           Set.fold flagtids ~init:st ~f:(fun st flagtid ->
               simultaneous_add ~user:flagtid ~used:tid st))
  
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

  let step _numrepeats _node data1 data2 =
    merge data1 data2
end

include T

let emp_tidset = Tidset.empty

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
  let outer_tidset = Tidset.singleton outer_tid in
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
