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

    let simple_operand_to_string (so : simple_operand) : string =
      match so with
      | Var v -> v
      | Num w -> Format.sprintf "%a" Word.pps w

    let rec directive_cnd_to_string (c : cnd) : string =
      match c with
      | Eq (c1, c2) -> Format.sprintf "%s = %s"
                         (simple_operand_to_string c1)
                         (simple_operand_to_string c2)
      | Lt (c1, c2) -> Format.sprintf "%s < %s"
                         (simple_operand_to_string c1)
                         (simple_operand_to_string c2)
      | And (c1, c2) -> Format.sprintf "%s && %s"
                         (directive_cnd_to_string c1)
                         (directive_cnd_to_string c2)

    let directive_to_string (d : directive) : string =
      match d with
      | Empty -> "empty"
      | Value c -> Format.sprintf "value_split(%s)"
                     (directive_cnd_to_string c)
      | Jmp c -> Format.sprintf "jmp_split(%s)"
                   (directive_cnd_to_string c)
      | Combine tids -> Format.sprintf "combine(%s)"
                          (Set.to_list tids |> List.to_string ~f:Tid.to_string)

    let to_string ((tids, directive) : tagged_directive) : string =
      let tid_string = Set.to_list tids |> List.to_string ~f:Tid.to_string in
      Format.sprintf "<%s, %s>" tid_string (directive_to_string directive)
  end

  include T

  type directive_map = t Tid_map.t

  let directive_and (left : directive) (right : directive) : directive Or_error.t =
    match left, right with
    | Empty, Empty -> Ok Empty
    | Value c1, Value c2 -> Ok (Value (And (c1, c2)))
    | Jmp c1, Jmp c2 -> Ok (Jmp (And (c1, c2)))
    | Combine c1, Combine c2 -> Ok (Combine (Tidset.union c1 c2))
    | _ ->
       let lefts = directive_to_string left in
       let rights = directive_to_string right in
       let err_s = "Can't combine incompatible directives: " ^ lefts ^ " and " ^ rights in
       Or_error.error_string err_s

  let tagged_directive_and ((tids1, dir1) : tagged_directive)
        ((tids2, dir2) : tagged_directive) : tagged_directive Or_error.t =
    Or_error.bind (directive_and dir1 dir2) ~f:(fun dir ->
        Ok (Tidset.union tids1 tids2, dir))

  let reduce_and_tagged_dirs (base : tagged_directive) (tds : tagged_directive list)
      : tagged_directive Or_error.t =
    let open Or_error.Monad_infix in
    List.fold tds ~init:(Ok base) ~f:(fun total_dir next_dir ->
        total_dir >>= fun dir -> tagged_directive_and dir next_dir)
  
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

      let is_var : Bil.exp -> bool = function
        | Bil.Var _ -> true
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
             let dir_var = Var (Var.name v) in
             let dir_num = Num w in
             (match normalize_comparator comparator with
              | Some Bil.EQ -> Some (Tidset.singleton tid,
                                     Value (Eq (dir_var, dir_num)))
              | _ -> None)
          | Bil.BinOp (comparator, Bil.Var v, Bil.Var x) ->
             let dir_v = Var (Var.name v) in
             let dir_x = Var (Var.name x) in
             (match normalize_comparator comparator with
              | Some Bil.EQ -> Some (Tidset.singleton tid,
                                     Value (Eq (dir_v, dir_x)))
              | _ -> None)
          | _ -> None
    end

    type prereq = {
        tidmap : Blk.elt Tid_map.t;
        dep_analy : (Calling_context.t, Dependency_analysis.t) Solution.t;
        use_analy : Dependency_analysis.t
      }

    let init (tidmap : Blk.elt Tid_map.t)
          (dep_analy : (Calling_context.t, Dependency_analysis.t) Solution.t)
          (use_analy : Dependency_analysis.t)
        : prereq =
      {tidmap;dep_analy; use_analy}

    let vars_of_simple_cnd = Var_name_collector.run

    let to_directive_map (dirs : t list) : directive_map =
      let emp = Tid_map.empty in
      List.fold dirs ~init:emp ~f:(fun dmap (tidset,dir) ->
          Set.to_list tidset
          |> List.fold ~init:dmap ~f:(fun dmap tid ->
                 Tid_map.set dmap ~key:tid ~data:dir))

    let last_deftid_is_simple_assign (p : prereq) (deftid : tid) : bool =
      match Tid_map.find p.tidmap deftid with
      | Some (`Def d) when Grammar.is_var (Def.rhs d) -> true
      | _ -> false

    let directive_if_simple_assign (p : prereq) (deftid : tid)
        : tagged_directive option =
      match Tid_map.find p.tidmap deftid with
      | Some (`Def d) when Grammar.is_var (Def.rhs d) ->
         let lhs = T.Var (Def.lhs d |> Var.name) in
         let rhs = (match Def.rhs d with
           | Bil.Var v -> T.Var (Var.name v)
           | _ -> failwith "shouldn't happen in directive_if_simple_assign")
         in
         let tidset = Tidset.singleton deftid in
         Some (tidset, T.Value (T.Eq (lhs, rhs)))
      | _ -> None

    let get_last_deftid (p : prereq) ~(var : string) ~(attid : tid)
        : tid option =
      let cc_attid = Calling_context.of_tid attid in
      let dep = Solution.get p.dep_analy cc_attid in
      let last_def_map = dep.last_defd_env in
      match Dependency_analysis.Varmap.find last_def_map var with
      | Some tidset when 1 = Tidset.length tidset ->
         Tidset.to_list tidset |> List.hd
      | _ -> None

    let try_get_second_level_directives (p : prereq) (flag_tid : tid)
          (flag_base_exp : Bil.exp) : tagged_directive list =
      let open Option.Monad_infix in
      let vars_of_base_exp = vars_of_simple_cnd flag_base_exp
                             |> SS.to_list
      in
      List.fold vars_of_base_exp ~init:[] ~f:(fun dirs used_var ->
          let maybe_new_dir =
            get_last_deftid p ~var:used_var ~attid:flag_tid >>=
              directive_if_simple_assign p
          in
          match maybe_new_dir with
          | Some dir -> dir :: dirs
          | None -> dirs)

    let get_merge_point_for_flag_dirs (p : prereq)
          ((tid,flagname) : tid * string)
          (flagdirs : t) : t option =
      match Tid_map.find p.use_analy.tid_users tid with
      | Some imm_flag_deps ->
         let () = printf "Flag %s at %a deps on:\n%!" flagname Tid.ppo tid;
                  Tidset.to_list imm_flag_deps |> List.iter
                                                    ~f:(printf "\t%a\n%!"
                                                          Tid.ppo)
         in
         let flag_dep_deps = Tidset.to_list imm_flag_deps in
         let flag_dep_deps = List.map flag_dep_deps ~f:(fun dep ->
                                 match Tid_map.find p.use_analy.tid_users dep with
                                 | Some fnd -> fnd
                                 | None -> Tidset.empty)
         in
         let flag_dep_deps = List.map flag_dep_deps ~f:Tidset.to_list in
         let flag_dep_deps = List.join flag_dep_deps in
         let flag_dep_deps = List.sort flag_dep_deps ~compare:Tid.compare
                             |> List.rev
         in
         let () = printf "all flag dep deps are:\n%!";
                  List.iter flag_dep_deps ~f:(printf "\t%a\n%!" Tid.ppo)
         in
         let latest_dep = List.hd_exn flag_dep_deps in
         let latest_depset = Tidset.singleton latest_dep in
         let tagged_combine_dir = (latest_depset, Combine (fst flagdirs)) in
         Some tagged_combine_dir
      | None -> None
      
    let get_conds_for_flag ((tid,flagname) : tid * string) (p : prereq) : t =
      let flag_tidset = Tidset.singleton tid in
      let flag_set_dir = flag_tidset, Value (Eq (Var flagname, Num (Word.one 1))) in
      let flag_exp = match Tid_map.find p.tidmap tid with
        | Some (`Def d) -> Def.rhs d
        | _ -> failwith @@
                 Format.sprintf "Couldn't find def of flag %s (%a)"
                   flagname
                   Tid.pps tid
      in
      (* if the exp setting the flag is simple, then also add it to the condition *)
      if Grammar.is_simple_cnd flag_exp
      then
        match Compiler.run flag_exp tid with
        | Some basedir ->
           (match tagged_directive_and basedir flag_set_dir with
            | Ok flag_set_dir ->
               let sec_lvl_dirs = try_get_second_level_directives p tid flag_exp in
               if List.is_empty sec_lvl_dirs
               then
                 flag_set_dir
               else
                 (match reduce_and_tagged_dirs flag_set_dir sec_lvl_dirs with
                  | Ok d -> d
                  | Error e -> failwith (Error.to_string_hum e))
            | Error e ->
               let () = printf "[Trace] Couldn't combine basedir and flag_set_dir of flag %s (%a)\n%!" flagname Tid.ppo tid
               in
               flag_set_dir)
        | None ->
           let () = printf "[Trace] Couldn't get basedir from def of flag %s (%a)\n%!"
                      flagname Tid.ppo tid
           in
           flag_set_dir
      else
        flag_set_dir
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
