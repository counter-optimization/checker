open Core
open Bap.Std
open Graphlib.Std

module SS = Common.SS

module Directives(N : Abstract.NumericDomain)
         (E : Abstract.MemoryT with type v := N.t
                                and type region := Common.Region.t
                                and type regions := Common.Region.Set.t
                                and type valtypes := Common.cell_t) = struct
  
  module T = struct
    type simple_operand = Var of string | Num of Word.t [@@deriving compare, sexp]
    
    type cnd = Eq of simple_operand * simple_operand
             | Lt of simple_operand * simple_operand
             | And of cnd * cnd
                              [@@deriving compare, sexp]
    
    type directive = Empty
                   | Value of cnd
                   | Jmp of cnd
                   | Combine of Tidset.t [@@deriving compare, sexp]

    type tagged_directive = Tidset.t * directive [@@deriving compare, sexp]

    type t = tagged_directive [@@deriving compare, sexp]
  end

  include T

  module Extractor = struct
    module Grammar = struct
      let is_var_or_num : Bil.exp -> bool = function
        | Bil.Var _ -> true
        | Bil.Int _ -> true
        | _ -> false

      let is_comparator_binop : binop -> bool = function
        | Bil.EQ | Bil.NEQ | Bil.LT | Bil.LE | Bil.SLT | Bil.SLE -> true
        | _ -> false

      let is_simple_cnd : Bil.exp -> bool = function
        | Bil.BinOp (op, left, right) ->
           is_comparator_binop op &&
             is_var_or_num left &&
               is_var_or_num right
        | _ -> false      
    end

    module Compiler = struct
      let normalize_comparator (comp : binop) : binop option =
        match comp with
        | Bil.EQ -> Some Bil.EQ
        | _ -> None
      
      let run (exp : Bil.exp) (tid : tid) : t option =
        if not @@ Grammar.is_simple_cnd exp
        then None
        else
          match exp with
          | Bil.BinOp (comparator, Bil.Var v, Bil.Int w)
            | Bil.BinOp (comparator, Bil.Int w, Bil.Var v) ->
             let dir_var = Var v in
             let dir_num = Num w in
             (match normalize_comparator comparator with
              | Some Bil.EQ -> Some (tid, Value (Eq dir_var dir_num))
              | _ -> None)
          | Bil.BinOp (comparator, Bil.Var v, Bil.Var x) ->
             let dir_v = Var v in
             let dir_x = Var x in
             (match normalize_comparator comparator with
              | Some Bil.EQ -> Some (tid, Value (Eq dir_v dir_x))
              | _ -> None)
    end

    type prereq = {
        tidmap : Blk.elt Tid_map.t;
        dep_analy : (Calling_context.t, Dependency_analysis.t) Solution.t
      }

    let init (tidmap : Blk.elt Tid_map.t)
          (dep_analy : (Calling_context.t, Dependency_analysis.t) Solution.t)
        : prereq =
      {tidmap;dep_analy}

    let vars_of_simple_cnd = Var_name_collector.run

    (* Given (flagtid, flagname), produce a directive can be used by the
       abstract interpreter to narrow the set of reaching states on either
       side of the branch *)
    let ok = 4

    (* let get_last_defs (simpl_cnd_vars : string list) (p : prereq) : def term list = *)
    (*   let  *)
  end
end

module Tree(N : Abstract.NumericDomain)
         (E : Abstract.MemoryT with type v := N.t
                                and type region := Common.Region.t
                                and type regions := Common.Region.Set.t
                                and type valtypes := Common.cell_t) = struct
  
  module Directives = Directives(N)(E)
    
  module T = struct
    type node = Leaf of E.t | Parent of {
                    left : node;
                    directive : Directives.t;
                    right : node;
                  }

    type t = node
  end
  
  include T

  let tids_overlap tids1 tids2 =
    not @@ Tidset.is_empty @@ Tidset.inter tids1 tids2

  let combine_partitions (tree : t) (combine_dir : Directives.directive) : t =
    let tids = match combine_dir with
      | Combine tids -> tids
      | _ -> failwith "Not combine directive in combine_partitions"
    in
    (* Combine all subtrees into one leaf when found=true *)
    let rec loop ?(found : bool = false) : t -> t = function
      | Leaf n -> Leaf n
      | Parent { left; directive; right } ->
         let (tid_tags, dir) = directive in
         let found = found || tids_overlap tids tid_tags in
         if found
         then
           match loop ~found left, loop ~found right with
           | Leaf l, Leaf r -> Leaf (E.merge l r)
           | _, _ ->
              failwith "loop in combine_partitions didn't return leaves"
         else
           Parent {
               left = loop ~found left;
               right = loop ~found right;
               directive
             }
    in
    loop tree
    

  let rec equal (left : t) (right : t) : bool =
    match left, right with
    | Leaf l, Leaf r -> E.equal l r
    | Parent l, Parent r ->
       let directive_eq = 0 = (Directives.compare l.directive r.directive) in
       directive_eq && equal l.left r.left && equal l.right r.right
    | _ -> false

  let rec map ~(f : E.t -> E.t) = function
    | Leaf node -> Leaf (f node)
    | Parent { left; directive; right } ->
       let left = map left ~f in
       let right = map right ~f in
       Parent { left; directive; right }

  let rec left_fold_join ~(f : E.t -> N.t) = function
    | Leaf node -> f node
    | Parent { left; directive; right } ->
       let left = left_fold_join ~f left in
       let right = left_fold_join ~f right in
       N.join left right
end

module Env(N : Abstract.NumericDomain)
         (E : Abstract.MemoryT with type v := N.t
                                and type region := Common.Region.t
                                and type regions := Common.Region.Set.t
                                and type valtypes := Common.cell_t) = struct

  module Tree = Tree(N)(E)

  module T = struct
    type env = {
        tree : Tree.t
      }

    type t = env
  end

  include T

  let of_mem (m : E.t) : t =
    { tree = Leaf m }

  let equal (l : t) (r : t) : bool =
    Tree.equal l.tree r.tree
end

module ConditionFinder = struct
  type rpo_traversal = Calling_context.t Seq.t

  type tidmap = Edge_builder.tidmap

  type dep_analysis = Dependency_analysis.t

  type prereqs = {
      rpo_traversal : rpo_traversal;
      tidmap : tidmap;
      dep_analysis : dep_analysis
    }

  type flag_name = string
  
  type live_flag = tid * flag_name

  type t = prereqs

  let init ~rpo_traversal ~tidmap ~dep_analysis : t =
    { rpo_traversal; tidmap; dep_analysis }

  module FlagScraper = struct
    (* hopefully/probably *)
    let flag_used_in_cmov ((tid, flagname) : live_flag) (prereqs : prereqs) : bool =
      let rec loop (exp : Bil.exp) : bool =
        match exp with
        | Bil.Load (_, offs, _, _) -> loop offs
        | Bil.Store (_, offs, data, _, _) -> loop offs || loop data
        | Bil.BinOp (_, l, r) -> loop l || loop r
        | Bil.UnOp (_, l) -> loop l
        | Bil.Var _ -> false
        | Bil.Int _ -> false
        | Bil.Cast (_, _, subexp) -> loop subexp
        | Bil.Let (v, exp, body) -> loop exp || loop body
        | Bil.Unknown (_, _) -> false
        | Bil.Ite (i, t, e) ->
           let all_names = SS.union SS.empty (Var_name_collector.run i)
                           |> SS.union (Var_name_collector.run t)
                           |> SS.union (Var_name_collector.run e)
           in
           SS.mem all_names flagname
        | Bil.Extract (_, _, subexp) -> loop subexp
        | Bil.Concat (l, r) -> loop l || loop r
      in
      let rec used_in_cmov_ever (tids : tid Seq.t) : bool =
        if Seq.is_empty tids
        then false
        else 
          let hd = Seq.hd_exn tids in
          let tids = Seq.tl_eagerly_exn tids in
          match Tid_map.find prereqs.tidmap hd with
          | Some (`Def d) ->
             let rhs = Def.rhs d in
             let used_in_cmov = loop rhs in
             used_in_cmov || used_in_cmov_ever tids
          | _ -> used_in_cmov_ever tids
      in
      let all_users = Dependency_analysis.users_transitive_closure
                        tid
                        prereqs.dep_analysis
                      |> Tidset.to_sequence
      in
      let () = printf "Users of %a are:\n%!" Tid.ppo tid;
               Seq.iter all_users ~f:(printf "\t%a\n%!" Tid.ppo)
      in
      used_in_cmov_ever all_users

    let get_live_flags (prereqs : prereqs) : live_flag list =
      let get_live_flags cc =
        let tid = Calling_context.to_insn_tid cc in
        match Tid_map.find prereqs.tidmap tid with
        | Some (`Def d) ->
           let defines = Var.name @@ Def.lhs d in
           let is_flag = Common.AMD64SystemVABI.var_name_is_flag defines in
           if is_flag && Dependency_analysis.has_user tid prereqs.dep_analysis
           then
             Some (tid, defines)
           else
             None
        | _ -> None
      in
      Seq.fold prereqs.rpo_traversal ~init:[] ~f:(fun liveflags cc ->
          match get_live_flags cc with
          | Some lfs -> lfs :: liveflags
          | None -> liveflags)
  end
end

module AbsInt = struct
  module Make(N : Abstract.NumericDomain)
           (E : Abstract.MemoryT with type v := N.t
                                  and type region := Common.Region.t
                                  and type regions := Common.Region.Set.t
                                  and type valtypes := Common.cell_t) = struct
    
    module Tree = Tree(N)(E)
    module TreeEnv = Env(N)(E)

    module Vt = struct type t = Common.cell_t end
    module BaseInt = Abstract.AbstractInterpreter(N)(Common.Region)(Common.Region.Set)(Vt)(E)

    type env = TreeEnv.t
    
    let denote_def (subname : string) (d : def term) (st : env) : env =
      let tree' = Tree.map ~f:(BaseInt.denote_def subname d) st.tree in
      { tree = tree' }

    let denote_phi (subname : string) (p : phi term) (st : env) : env =
      let tree' = Tree.map ~f:(BaseInt.denote_phi subname p) st.tree in
      { tree = tree' }

    let denote_jmp (subname : string) (j : jmp term) (st : env) : env =
      let tree' = Tree.map ~f:(BaseInt.denote_jmp subname j) st.tree in
      { tree = tree' }

    let denote_elt (subname : string) (e : Blk.elt) (st : env) : env =
      match e with
      | `Def d -> denote_def subname d st
      | `Jmp j -> denote_jmp subname j st
      | `Phi p -> denote_phi subname p st
  end
end
