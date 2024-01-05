open Core_kernel
open Bap.Std
open Graphlib.Std

type varname = string [@@deriving compare, sexp]

module Def = struct
  type t = tid option * varname [@@deriving compare, sexp]

  let tid : t -> tid option = fst
  let var : t -> varname = snd
end

module Cmp = struct
  include Def
  include Comparator.Make(Def)
end

module DefSet = Set.Make_using_comparator(Cmp)

type def = Def.t

type defset = DefSet.t

type sol = (Tid.t, defset) Solution.t

let tidmap_of_sol (s : sol) : DefSet.t Tid.Map.t =
  Tid.Map.of_alist_exn @@
  Seq.to_list @@
  Solution.enum s

let sol_of_tidmap (tm : DefSet.t Tid.Map.t) : sol =
  Solution.create tm DefSet.empty

let sexp_of_sol (s : sol) : Sexp.t =
  Tid.Map.sexp_of_t DefSet.sexp_of_t @@ tidmap_of_sol s

let sol_of_sexp (sexp : Sexp.t) : sol =
  let m = Tid.Map.t_of_sexp DefSet.t_of_sexp sexp in
  Solution.create m DefSet.empty

let compare_sol (x : sol) (y : sol) : int =
  let xm = tidmap_of_sol x in
  let ym = tidmap_of_sol y in
  Tid.Map.compare DefSet.compare xm ym

let equal_sol : sol -> sol -> bool =
  Solution.equal ~equal:DefSet.equal

(** rd is reaching def analysis result 
    users_of takes a tid, t, and returns a set of tids identifying
    terms that use t 
    term_uses takes a tid, t, and returns a set of tids that t
    uses *)
type t = {
  rd : sol;
  users_of : Tidset.t ref Tid_map.t;
  term_uses : Tidset.t ref Tid_map.t;
} [@@deriving sexp, compare, equal]

let empty : t =
  { rd = Solution.create Tid.Map.empty DefSet.empty;
    users_of = Tid.Map.empty;
    term_uses = Tid.Map.empty }

let join (left : t) (right : t) : t =
  let mapjoin (type a)
        (x : a Tid.Map.t)
        (y : a Tid.Map.t)
        ~(join : a -> a -> a) : a Tid.Map.t =
    Tid.Map.merge x y ~f:(fun ~key -> function
      | `Left l -> Some l
      | `Right r -> Some r
      | `Both (l, r) -> Some (join l r))
  in
  let ref_join l r : Tid.Set.t ref =
    ref (Tid.Set.union !l !r)
  in
  let ldefs = tidmap_of_sol left.rd in
  let rdefs = tidmap_of_sol right.rd in
  { rd = sol_of_tidmap @@
      mapjoin ldefs rdefs ~join:DefSet.union;
    users_of = mapjoin left.users_of right.users_of
                 ~join:ref_join;
    term_uses = mapjoin left.term_uses right.term_uses
                  ~join:ref_join }

let tids_of_defs defset =
  DefSet.to_list defset
  |> List.map ~f:Def.tid
  |> List.filter ~f:Option.is_some
  |> List.map ~f:(fun x -> Option.value_exn x)

(* nonreflexive: a tid doesn't use itself *)
let users_transitive_closure st origtid =
  let rec loop ?(processed : Tidset.t = Tidset.empty) toproc =
    match toproc with
    | [] -> processed
    | tid :: rst ->
      if Tidset.mem processed tid
      then loop ~processed rst
      else
        let processed = Tidset.add processed tid in
        match Tid_map.find st.users_of tid with
        | None -> loop ~processed rst
        | Some users ->
          let nonproc_users = Tidset.diff !users processed
                              |> Tidset.to_list in
          let toproc = List.append toproc nonproc_users in
          loop ~processed toproc
  in
  let users_plus_self = loop [origtid] in
  let users = Tidset.remove users_plus_self origtid in
  users

let select_tids ~defset ~select =
  DefSet.filter defset ~f:(fun def ->
    let name = Def.var def in
    Common.SS.mem select name)
  |> DefSet.to_list
  |> List.map ~f:Def.tid
  |> List.filter ~f:Option.is_some
  |> List.map ~f:(fun x -> Option.value_exn x)

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

let init_sol (type g d) (module G : Graph with type t = g and type node = Tid.t) sub cfg_firstnode =
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

let get_uses_and_users sol tidmap flagowners : t =
  let term_uses : Tidset.t ref Tid_map.t ref = ref Tid_map.empty in
  let users_of : Tidset.t ref Tid_map.t ref = ref Tid_map.empty in
  let add_to_uses_and_users ~attid ~used =
    let uses = match Tid_map.find !term_uses attid with
      | Some uses -> uses
      | None ->
        let newuses = ref Tidset.empty in
        term_uses := Tid_map.set !term_uses
                       ~key:attid ~data:newuses;
        newuses
    in
    uses := Tidset.add !uses used;
    let users = match Tid_map.find !users_of used with
      | Some users -> users
      | None ->
        let newusers = ref Tidset.empty in
        users_of := Tid_map.set !users_of
                      ~key:used ~data:newusers;
        newusers
    in
    users := Tidset.add !users attid
  in
  let set_flags ~attid =
    let flagtids = Flag_ownership.get_flags_of_def_tid
                  flagowners attid in
    Set.iter flagtids ~f:(fun ft ->
      add_to_uses_and_users ~attid:ft ~used:attid)
  in
  let rhs tid =
    match Tid_map.find tidmap tid with
    | Some elt -> Var_name_collector.run_on_elt ~rhsonly:true elt
    | None ->
      Format.sprintf "[Rdefs] couldn't find tid %a in tidmap" Tid.pps tid
      |> failwith
  in
  let process_sol_node (tid, defs) =
    let rhs = rhs tid in
    let uses_tids = select_tids ~defset:defs ~select:rhs in
    List.iter uses_tids ~f:(fun used ->
      add_to_uses_and_users ~attid:tid ~used;
      set_flags ~attid:tid)
  in
  (Solution.enum sol
   |> Seq.iter ~f:process_sol_node);
  { rd = sol;
    term_uses = !term_uses;
    users_of = !users_of }

let run_on_cfg (type g) (module G : Graph with type t = g and type node = Tid.t) g sub tidmap flagownership cfg_firstnode =
  let interp_node tid =
    let elt = match Tid_map.find tidmap tid with
      | Some elt -> elt
      | None -> failwith @@
        sprintf
          "in Reachingdefs.run_on_cfg, couldn't find tid %a in tidmap"
          Tid.pps tid
    in
    denote_elt elt
  in
  let merge = DefSet.union in
  let equal = DefSet.equal in
  let widen_with_step _nvisits _node = DefSet.union in
  let init_sol = init_sol (module G) sub cfg_firstnode in
  let rd = Graphlib.fixpoint
             (module G)
             g
             ~step:widen_with_step
             ~init:init_sol
             ~start:cfg_firstnode
             ~equal
             ~merge
             ~f:interp_node
  in
  let full_result = get_uses_and_users rd tidmap flagownership in
  full_result

let get_last_defs st ~attid ~forvar =
  let reachingdefs = Solution.get st.rd attid in
  let forvar_def = select_tids
                     ~defset:reachingdefs
                     ~select:(Common.SS.singleton forvar) in
  Tidset.of_list forvar_def

let has_users st tid =
  let trans_users = users_transitive_closure st tid in
  Tidset.length trans_users >= 1

let get_users st tid =
  match Tid_map.find st.users_of tid with
  | Some tids -> !tids
  | None -> Tidset.empty

let get_uses st tid =
  match Tid_map.find st.term_uses tid with
  | Some tids -> !tids
  | None -> Tidset.empty
