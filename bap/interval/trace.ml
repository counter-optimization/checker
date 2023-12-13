open Core_kernel
open Bap.Std
open Graphlib.Std

module SS = Common.SS

let log_prefix = sprintf "%s.trace" Common.package
module L = struct
  include Dolog.Log
  let () = set_prefix log_prefix
end

module Directives(N : Abstract.NumericDomain)
    (E : Abstract.MemoryT with type v := N.t
                           and type region := Common.Region.t
                           and type regions := Common.Region.Set.t
                           and type valtypes := Common.cell_t) = struct

  let get_intvl : N.t -> Wrapping_interval.t =
    match N.get Wrapping_interval.key with
    | Some f -> f
    | None -> failwith "[TraceDir] Couldn't extract interval information"

  let set_intvl (v : N.t) (wi : Wrapping_interval.t) : N.t =
    N.set Wrapping_interval.key v wi

  let get_taint : N.t -> Checker_taint.Analysis.t =
    match N.get Checker_taint.Analysis.key with
    | Some f -> f
    | None -> failwith "[TraceDir] Couldn't extract taint information"

  let set_taint (v : N.t) (t : Checker_taint.Analysis.t) : N.t =
    N.set Checker_taint.Analysis.key v t

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

    let is_var = function
      | Var _ -> true
      | _ -> false

    let directive_equal : directive -> directive -> bool = fun left right ->
      0 = compare_directive left right

    let equal : t -> t -> bool = fun left right ->
      0 = compare left right

    let is_combine : directive -> bool = function
      | Combine _ -> true
      | _ -> false

    let cnd (td : tagged_directive) : cnd option =
      let (tidset, dir) = td in
      match dir with
      | Jmp c
      | Value c -> Some c
      | _ -> None

    let is_tagged_combine (tdir : t) : bool =
      is_combine @@ snd tdir

    let get_dir : t -> directive = snd
    let get_tags : t -> Tidset.t = fst
    let get_tids : t -> Tidset.t = get_tags

    let simple_operand_to_string (so : simple_operand) : string =
      match so with
      | Var v -> v
      | Num w -> Format.sprintf "%a" Word.pps w

    let rec cnds_as_substs (c : cnd) : (string * string) list =
      match c with
      | Eq (Var l, Var r) -> [(l, r)]
      | Eq (_, _) -> []
      | Lt (_, _) -> []
      | And (c1, c2) ->
        List.append (cnds_as_substs c1) (cnds_as_substs c2)

    let rec subst_cnd (target : cnd) ((replace, with_) : string * string) : cnd =
      let s = function
        | Var v -> if String.equal v replace
          then Var with_
          else Var v
        | Num n -> Num n in
      match (target : cnd) with
      | Eq (l, r) -> Eq (s l, s r)
      | Lt (l, r) -> Lt (s l, s r)
      | And (l, r) -> And (subst_cnd l (replace, with_), subst_cnd r (replace, with_))

    let substs_of_tdir ((tags, dir) : tagged_directive) : (string * string) list =
      match dir with
      | Value c -> cnds_as_substs c
      | Jmp c -> cnds_as_substs c
      | Empty -> []
      | Combine _ -> []

    let rec subst_tdir ((tags, dir) : t) (subst : string * string) : t =
      match dir with
      | Empty -> tags, Empty
      | Value cnd -> tags, Value (subst_cnd cnd subst)
      | Jmp cnd -> tags, Jmp (subst_cnd cnd subst)
      | Combine x -> tags, Combine x

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

  (* directive_map handling *)
  type directive_map = tagged_directive Tid_map.t

  let tid_has_directive : directive_map -> tid -> bool = Tid_map.mem

  let get_tdirective : directive_map -> tid -> tagged_directive = Tid_map.find_exn

  (* operations on directives *)
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

  module Applier = struct

    module WI = Wrapping_interval

    type pos_neg_applier = (E.t -> E.t) * (E.t -> E.t)

    let id : 'a. 'a -> 'a = fun x -> x
    let id_applier : pos_neg_applier = id, id

    let rec eval_cnd : cnd -> pos_neg_applier =
      let propagate_taint oldleft oldright newval =
        let lt = get_taint oldleft in
        let rt = get_taint oldright in
        let new_wi = get_intvl newval in
        let final_taint = if WI.equal new_wi WI.bot
          then Checker_taint.Analysis.Notaint
          else Checker_taint.Analysis.join lt rt in
        set_taint newval final_taint in
      fun cnd ->
        match cnd with
        | Eq (Var a, Var b) ->
          let pos_applier = fun (env : E.t) ->
            let a_val = E.lookup a env in
            let b_val = E.lookup b env in
            let final_val = N.meet a_val b_val
                            |> propagate_taint a_val b_val in
            E.set a final_val env
            |> E.set b final_val in
          let neg_applier = fun (env : E.t) ->
            let a_val = E.lookup a env in
            let b_val = E.lookup b env in
            let a_wi = get_intvl a_val in
            let b_wi = get_intvl b_val in
            let a_wi' = WI.try_remove_interval ~remove:b_wi ~from_:a_wi in
            let b_wi' = WI.try_remove_interval ~remove:a_wi ~from_:b_wi in
            let a_val' = set_intvl a_val a_wi'
                         |> propagate_taint a_val b_val in
            let b_val' = set_intvl b_val b_wi'
                         |> propagate_taint a_val b_val in
            E.set a a_val' env 
            |> E.set b b_val' in
          (pos_applier, neg_applier)
        | Eq (Var v, Num w)
        | Eq (Num w, Var v) ->
          let const = N.of_word w in
          if Common.AMD64SystemVABI.var_name_is_flag v
          then
            let one = Word.one @@ Word.bitwidth w in
            let zero = Word.zero @@ Word.bitwidth w in
            let pos_val, neg_val = if Word.equal w one
              then (N.of_word one, N.of_word zero)
              else (N.of_word zero, N.of_word one) in
            let pos_applier = E.set v pos_val in
            let neg_applier = E.set v neg_val in
            (pos_applier, neg_applier)
          else
            let pos_applier = fun (env : E.t) ->
              let var_val = E.lookup v env in
              let intersect = N.meet var_val const
                              |> propagate_taint var_val const in
              E.set v intersect env in
            let neg_applier = fun (env : E.t) ->
              let const_wi = get_intvl const in
              let var_val = E.lookup v env in
              let var_wi = get_intvl var_val in
              let var_wi' = WI.try_remove_interval ~remove:const_wi ~from_:var_wi in
              let var_val' = set_intvl var_val var_wi'
                             |> propagate_taint var_val const in
              E.set v var_val' env in
            (pos_applier, neg_applier)
        | Lt (Var a, Var b) ->
          let pos_applier = fun (env : E.t) ->
            failwith "LT (VAR A, VAR B) not supported in Directives.Applier.eval_cnd yet" in
          let neg_applier = fun (env : E.t) ->
            failwith "LT (VAR A, VAR B) not supported in Directives.Applier.eval_cnd yet" in
          (pos_applier, neg_applier)
        | Lt (Var v, Num w)
        | Lt (Num w, Var v) ->
          let pos_applier = fun (env : E.t) ->
            failwith "LT (VAR A, NUM B) not supported in Directives.Applier.eval_cnd yet" in
          let neg_applier = fun (env : E.t) ->
            failwith "LT (VAR A, NUM B) not supported in Directives.Applier.eval_cnd yet" in
          (pos_applier, neg_applier)
        | And (c1, c2) ->
          let (p1, n1) = eval_cnd c1 in
          let (p2, n2) = eval_cnd c2 in
          (fun e -> p2 (p1 e)), (fun e -> n2 (n1 e))
        | _ ->
          failwith
            "Fallthrough case in Directives.Applier.eval_cnd shouldn't happen"

    let build : tagged_directive -> pos_neg_applier =
      fun (tids, dir) ->
      match dir with
      | Value cnd -> eval_cnd cnd
      | Jmp cnd -> eval_cnd cnd
      | _ -> id_applier
  end

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
      rd : Reachingdefs.t;
    }

    let init tidmap rd : prereq =
      { tidmap; rd }

    let vars_of_simple_cnd = Var_name_collector.run

    let to_directive_map (dirs : t list) : directive_map =
      let emp = Tid_map.empty in
      List.fold dirs ~init:emp ~f:(fun dmap tdir ->
        Set.to_list (fst tdir)
        |> List.fold ~init:dmap ~f:(fun dmap tid ->
          Tid_map.set dmap ~key:tid ~data:tdir))

    let last_deftid_is_simple_assign (p : prereq) (deftid : tid) : bool =
      match Tid_map.find p.tidmap deftid with
      | Some (`Def d) when Grammar.is_var (Def.rhs d) -> true
      | _ -> false

    let directive_if_simple_assign (p : prereq) (flagtid : tid) (deftid : tid)
      : tagged_directive option =
      match Tid_map.find p.tidmap deftid with
      | Some (`Def d) when Grammar.is_var (Def.rhs d) ->
        let lhs = T.Var (Def.lhs d |> Var.name) in
        let rhs = (match Def.rhs d with
          | Bil.Var v -> T.Var (Var.name v)
          | _ -> failwith "shouldn't happen in directive_if_simple_assign") in
        let tidset = Tidset.singleton flagtid in
        Some (tidset, T.Value (T.Eq (lhs, rhs)))
      | _ -> None

    let get_last_deftid (p : prereq) ~(var : string) ~(attid : tid)
      : tid option =
      let last_defs = Reachingdefs.get_last_defs p.rd ~attid ~forvar:var in
      if Tidset.length last_defs = 1
      then Tidset.to_list last_defs |> List.hd
      else None

    let try_get_second_level_directives (p : prereq) (flag_tid : tid)
          (flag_base_exp : Bil.exp) : tagged_directive list =
      let open Option.Monad_infix in
      let vars_of_base_exp = vars_of_simple_cnd flag_base_exp
                             |> SS.to_list in
      List.fold vars_of_base_exp ~init:[] ~f:(fun dirs used_var ->
        let maybe_new_dir =
          get_last_deftid p ~var:used_var ~attid:flag_tid >>= fun t ->
          directive_if_simple_assign p flag_tid t in
        match maybe_new_dir with
        | Some dir -> dir :: dirs
        | None -> dirs)

    let get_merge_point_for_flag_dirs (p : prereq)
          ((tid,flagname) : tid * string)
          (flagdirs : t) : t option =
      let imm_flag_deps = Reachingdefs.get_users p.rd tid in
      if Tidset.is_empty imm_flag_deps
      then None
      else
        let flag_dep_deps = Tidset.to_list imm_flag_deps in
        let flag_dep_deps = List.map flag_dep_deps ~f:(Reachingdefs.get_users p.rd) in
        let flag_dep_deps = List.map flag_dep_deps ~f:Tidset.to_list
                            |> List.join
                            |> List.sort ~compare:Tid.compare
                            |> List.rev in
        List.hd flag_dep_deps
        |> Option.bind ~f:(fun latest_dep ->
          let latest_depset = Tidset.singleton latest_dep in
          let tagged_combine_dir = (latest_depset, Combine (fst flagdirs)) in
          Some tagged_combine_dir)

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
               let all_dirs = List.map sec_lvl_dirs ~f:(fun tdir ->
                 substs_of_tdir tdir)
                              |> List.join
                              |> List.map ~f:(fun snd_lvl_subst ->
                                subst_tdir flag_set_dir snd_lvl_subst)
               in
               (match reduce_and_tagged_dirs flag_set_dir all_dirs with
                | Ok d -> d
                | Error e -> failwith (Error.to_string_hum e))
           | Error e ->
             let () = printf "[Trace] Couldn't combine basedir and flag_set_dir of flag %s (%a)\n%!" flagname Tid.ppo tid
             in
             flag_set_dir)
        | None ->
          printf "[Trace] Couldn't get basedir from def of flag %s (%a)\n%!" flagname Tid.ppo tid;
          flag_set_dir
      else flag_set_dir
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

  let rec to_string (tree : t) : string =
    match tree with
    | Leaf l -> "(leaf)"
    | Parent { left; right; _ } ->
      Format.sprintf "(Parent %s %s)" (to_string left) (to_string right)

  let is_empty (tree : t) : bool =
    match tree with
    | Leaf env -> E.is_empty env
    | _ -> false

  let tids_overlap tids1 tids2 =
    not @@ Tidset.is_empty @@ Tidset.inter tids1 tids2

  let combine_partitions (tree : t) (combine_dir : Directives.directive) : t =
    let tids = match combine_dir with
      | Combine tids -> tids
      | _ -> failwith "Not combine directive in combine_partitions" in
    (* Combine all subtrees into one leaf when found=true *)
    let rec loop ?(found : bool = false) : t -> t = function
      | Leaf n -> Leaf n
      | Parent { left; directive; right } ->
        let (tid_tags, dir) = directive in
        let found = found || tids_overlap tids tid_tags in
        if found
        then match loop ~found left, loop ~found right with
          | Leaf l, Leaf r -> Leaf (E.merge l r)
          | _, _ ->
            failwith "loop in combine_partitions didn't return leaves"
        else Parent {
          left = loop ~found left;
          right = loop ~found right;
          directive
        } in
    loop tree

  let rec do_directive_split (tree : t) (tdir : Directives.tagged_directive) : t =
    match tree with
    | Leaf n ->
      let true_pruner, false_pruner = Directives.Applier.build tdir in
      let left_env = true_pruner n in
      let right_env = false_pruner n in
      let left = Leaf left_env in
      let right = Leaf right_env in
      Parent {
        left;
        right; 
        directive = tdir
      }
    | Parent { left; directive; right } ->
      Parent {
        left = do_directive_split left tdir;
        right = do_directive_split right tdir;
        directive;
      }

  let rec directive_already_applied (tree : t)
            (tdir : Directives.tagged_directive) : bool =
    let rec loop tree k =
      match tree with
      | Leaf l -> k false
      | Parent p -> if Directives.equal p.directive tdir
        then k true
        else loop p.left (fun leftres ->
          if leftres
          then k true
          else loop p.right (fun x -> x)) in
    loop tree (fun x -> x)

  let apply_directive (tree : t) (tdir : Directives.tagged_directive) : t =
    if Directives.is_tagged_combine tdir
    then combine_partitions tree @@ Directives.get_dir tdir
    else if directive_already_applied tree tdir
    then tree
    else do_directive_split tree tdir

  let rec equal (left : t) (right : t) : bool =
    let rec loop l r k =
      match l, r with
      | Leaf l, Leaf r -> k (E.equal l r)
      | Parent l, Parent r ->
        Directives.equal l.directive r.directive &&
        loop l.left r.left (fun left_res ->
          k @@ 
          left_res &&
          loop l.right r.right (fun right_res -> right_res))
      | _, _ -> k false in
    loop left right (fun x -> x)

  let map t ~(f : E.t -> E.t) : t =
    let rec loop t k =
      match t with
      | Leaf node -> k @@ Leaf (f node)
      | Parent { left; directive; right } ->
        loop left (fun left_tree ->
          loop right (fun right_tree ->
            k @@ Parent { left = left_tree;
                          right = right_tree;
                          directive })) in
    loop t (fun x -> x)

  let map2 l r ~(f : E.t -> E.t -> E.t) =
    let uneq_fail () =
      failwith "[Trace] Tree.map2 requires directives_equal trees" in
    let rec loop l r k =
      match l, r with
      | Leaf l, Leaf r -> k @@ Leaf (f l r)
      | Parent l, Parent r ->
        if not @@ Directives.equal l.directive r.directive
        then uneq_fail ()
        else
          loop l.left r.left (fun lefttree ->
            loop l.right r.right (fun righttree ->
              k @@ Parent { left = lefttree;
                            right = righttree;
                            directive = l.directive }))
      | _, _ -> uneq_fail () in
    loop l r (fun x -> x)

  let rec map_list : 'a. (E.t -> 'a) -> t -> 'a list = fun f tree ->
    match tree with
    | Leaf n -> [f n]
    | Parent { left; right; _ } ->
      List.append (map_list f left) (map_list f right)

  let rec left_fold_join ~(f : E.t -> N.t) = function
    | Leaf node -> f node
    | Parent { left; directive; right } ->
      let left = left_fold_join ~f left in
      let right = left_fold_join ~f right in
      N.join left right

  (* conditions are the same on both sides of the tree,
     so just collect along the left side *)
  let rec conditions (tree : t) : Directives.cnd list =
    match tree with
    | Leaf e -> []
    | Parent p ->
      (match Directives.cnd p.directive with
       | Some cnd -> cnd :: conditions p.left
       | None -> conditions p.left)

  let rec conditions_prefix ~(prefix : Directives.cnd list)
            ~(of_ : Directives.cnd list) : bool =
    match prefix, of_ with
    | [], _ -> true
    | _, [] -> false
    | x :: xs, y :: ys ->
      0 = Directives.compare_cnd x y &&
      conditions_prefix ~prefix:xs ~of_:ys

  (* directives are the same on both sides of the tree,
     so just collect along the left side *)
  let directives (tree : t) : Directives.t list =
    let rec loop subtree k =
      match subtree with
      | Leaf e -> []
      | Parent p ->
        loop p.left (fun dirs ->
          p.directive :: dirs) in
    loop tree (fun x -> x)

  let rec directives_prefix ~(prefix : Directives.t list)
            ~(of_ : Directives.t list) : bool =
    match prefix, of_ with
    | [], _ -> true
    | _, [] -> false
    | x :: prefixes, y :: ofs ->
      0 = Directives.compare x y &&
      directives_prefix ~prefix:prefixes ~of_:ofs

  let directives_equal (left : t) (right : t) : bool =
    let left = directives left in
    let right = directives right in
    List.equal Directives.equal left right

  (* precondition: Bool.equal true (directives ~prefix ~of_) *)
  let rec remove_directives_prefix ~(prefix : Directives.t list)
            ~(of_ : Directives.t list) : Directives.t list =
    match prefix, of_ with
    | [], _ -> of_
    | x :: prefixes, y :: ofs_ ->
      remove_directives_prefix ~prefix:prefixes ~of_:ofs_
    | _ ->
      failwith "[Trace] remove_directives_prefix, ~prefix not prefix of ~of_"

  let isomorphic_merge ?(meet = false) (left : t) (right : t) : t =
    let open Or_error.Monad_infix in
    let rec loop (left : t) (right : t) k : t Or_error.t =
      match left, right with
      | Leaf left, Leaf right -> k (Ok (Leaf (E.merge ~meet left right)))
      | Parent left, Parent right ->
        if not @@ Directives.equal left.directive right.directive
        then
          Or_error.error_string "[Trace] isomorphic merge does not support non-isomorphic trees (tree directives)"
        else
          let directive = left.directive in
          loop left.left right.left (Or_error.bind ~f:(fun l ->
            loop left.right right.right (Or_error.bind ~f:(fun r ->
              k (Ok (Parent { left=l; right=r; directive }))))))
      | _ ->
        Or_error.error_string "[Trace] tree merge does not support non-isomorphic trees (tree shape)"
    in
    match loop left right (fun x -> x) with
    | Ok res -> res
    | Error e ->
      let () = printf "Failed to merge two trees:\n\t1. %s\n\t2. %s\n%!"
                 (to_string left)
                 (to_string right) in
      failwith @@ Error.to_string_hum e

  let normalize_prefixed_trees ?(meet = false)
        ~(do_merge : bool)
        ~(suffix : Directives.t list)
        ~(target : t)
        ~(other : t) : t =
    let split tree tdir = if directive_already_applied tree tdir
      then tree
      else do_directive_split tree tdir in
    let refined_target = List.fold suffix
                           ~init:target
                           ~f:split in
    if do_merge
    then isomorphic_merge ~meet refined_target other
    else refined_target

  let merge ?(meet = false) (left : t) (right : t) : t =
    let pick_nonempty = match is_empty left, is_empty right with
      | true, true -> Some left
      | true, false -> Some right
      | false, true -> Some left
      | false, false -> None in
    match pick_nonempty with
    | Some t -> t
    | None ->
      let left_tdirs = directives left in
      let right_tdirs = directives right in
      if directives_prefix ~prefix:left_tdirs ~of_:right_tdirs
      then
        let remaining_suffix = remove_directives_prefix
                                 ~prefix:left_tdirs
                                 ~of_:right_tdirs in
        normalize_prefixed_trees
          ~meet
          ~do_merge:true
          ~suffix:remaining_suffix
          ~target:left
          ~other:right
      else
      if directives_prefix ~prefix:right_tdirs ~of_:left_tdirs
      then
        let remaining_suffix = remove_directives_prefix
                                 ~prefix:right_tdirs
                                 ~of_:left_tdirs in
        normalize_prefixed_trees
          ~meet
          ~do_merge:true
          ~suffix:remaining_suffix
          ~target:right
          ~other:left
      else
        (* neither refine each other, so pick (mid : t) s.t. 
           mid refines both left and right *)
        normalize_prefixed_trees
          ~meet
          ~do_merge:false
          ~suffix:right_tdirs
          ~target:left
          ~other:right

  let any_node (tree : t) ~(f : E.t -> bool) : bool =
    let rec loop tree k =
      match tree with
      | Leaf e -> k (f e)
      | Parent { left; right; _ } ->
        loop left (fun leftres ->
          loop right (fun rightres ->
            leftres || rightres))
    in
    loop tree (fun x -> x)

  let all_node (tree : t) ~(f : E.t -> bool) : bool =
    let rec loop tree k =
      match tree with
      | Leaf e -> k (f e)
      | Parent { left; right; _ } ->
        loop left (fun leftres ->
          loop right (fun rightres ->
            leftres && rightres))
    in
    loop tree (fun x -> x)

  let num_leaves tree : int =
    let rec loop tree k =
      match tree with
      | Leaf _ -> k 1
      | Parent { left; right; _ } ->
        loop left (fun leftsz ->
          loop right (fun rightsz ->
            leftsz + rightsz))
    in
    loop tree (fun x -> x)
end

module Env(N : Abstract.NumericDomain)
    (E : Abstract.MemoryT with type v := N.t
                           and type region := Common.Region.t
                           and type regions := Common.Region.Set.t
                           and type valtypes := Common.cell_t) = struct

  let get_intvl : N.t -> Wrapping_interval.t =
    match N.get Wrapping_interval.key with
    | Some f -> f
    | None -> failwith "Couldn't extract interval information in Trace.Env get_intvl"

  module Tree = Tree(N)(E)
  module Directives = Directives(N)(E)

  module T = struct
    type env = {
      tree : Tree.t;
    }

    type t = env
  end

  include T

  let empty : t = {
    tree = Tree.Leaf E.empty;
  }

  let default_with_env : E.t -> t = fun env ->
    { tree = Tree.Leaf env }

  let pp : t -> unit = fun { tree } ->
    let tdir_list_to_string ~(is_left : bool)
          (tdirlist : Directives.t list) : string =
      Format.sprintf "%s(%s)"
        (if is_left then "" else "~")
        (List.to_string tdirlist ~f:Directives.to_string)
    in
    let rec loop ?(tdirstack : Directives.t list = [])
              ~(is_left : bool)
              (tree : Tree.t) : unit =
      match tree with
      | Tree.Leaf env ->
        printf "%s:\n\t\t%!" (tdir_list_to_string ~is_left tdirstack);
        E.pp env
      | Tree.Parent p ->
        let tdirstack = p.directive :: tdirstack in
        loop ~tdirstack ~is_left:true p.left;
        loop ~tdirstack ~is_left:false p.right
    in
    loop tree ~is_left:true

  let of_mem (m : E.t) : t =
    { tree = Leaf m }

  let equal (l : t) (r : t) : bool =
    Tree.equal l.tree r.tree

  let merge ?(meet = false) (l : t) (r : t) : t =
    { tree = Tree.merge ~meet l.tree r.tree }

  let num_leaves (env : t) : int = Tree.num_leaves env.tree

  let widen_with_step ?(meet = false) (n : int) (node : 'a) (l : t) (r : t) : t =
    if n < Common.ai_widen_threshold
    then merge ~meet l r
    else if Tree.directives_equal l.tree r.tree
    then
      let tree = Tree.map2 l.tree r.tree ~f:(E.widen_with_step ~meet n node) in
      { tree }
    else
      failwith "[Trace] infinite loop stuck in widen_with_step"
end

module ConditionFinder = struct
  type rpo_traversal = Calling_context.t Seq.t

  type tidmap = Blk.elt Tid.Map.t

  type prereqs = {
    rpo_traversal : rpo_traversal;
    tidmap : tidmap;
    rd : Reachingdefs.t;
  }

  type flag_name = string

  type live_flag = tid * flag_name

  type t = prereqs

  let init ~rpo_traversal ~tidmap ~reachingdefs : t =
    { rpo_traversal; tidmap; rd = reachingdefs }

  module FlagScraper = struct
    let flag_is_checkbit ((tid, flagname) : live_flag) prereqs =
      let exp_is_dmp_bittest = function
        | Bil.Cast (low, 1, (Bil.BinOp (rshift, (Bil.Var v), (Bil.Int w)))) -> true
        | _ -> false in
      match Tid_map.find prereqs.tidmap tid with
      | None -> false
      | Some (`Def d) -> exp_is_dmp_bittest (Def.rhs d)
      | Some _ -> false
    
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
                          |> SS.union (Var_name_collector.run e) in
          SS.mem all_names flagname
        | Bil.Extract (_, _, subexp) -> loop subexp
        | Bil.Concat (l, r) -> loop l || loop r in
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
          | _ -> used_in_cmov_ever tids in
      let all_users = Reachingdefs.users_transitive_closure
                        prereqs.rd tid
                      |> Tidset.to_sequence in
      used_in_cmov_ever all_users

    let get_live_flags (prereqs : prereqs) : live_flag list =
      let get_live_flags cc =
        let tid = Calling_context.to_insn_tid cc in
        match Tid_map.find prereqs.tidmap tid with
        | Some (`Def d) ->
          let defines = Var.name @@ Def.lhs d in
          let is_flag = Common.AMD64SystemVABI.var_name_is_flag defines in
          if is_flag && Reachingdefs.has_users prereqs.rd tid
          then Some (tid, defines)
          else None
        | _ -> None in
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
    module Directives = Directives(N)(E)

    module Vt = struct type t = Common.cell_t end
    module BaseInt = Abstract.AbstractInterpreter(N)(Common.Region)(Common.Region.Set)(Vt)(E)

    type env = TreeEnv.t

    let nondet_denote_exp (exp : Bil.exp) (st : env) : N.t list =
      let base_denote_exp = fun env -> BaseInt.denote_exp exp env |> fst in
      Tree.map_list base_denote_exp st.tree

    let denote_def (dmap : Directives.directive_map)
          (d : def term) (st : env) : env =
      let tid = Term.tid d in
      let st : env =
        if Directives.tid_has_directive dmap tid
        then
          (L.debug "doing split at %a" Tid.ppo tid;
          let tdir = Directives.get_tdirective dmap tid in
          { tree = Tree.apply_directive st.tree tdir })
        else st in
      let tree = Tree.map ~f:(BaseInt.denote_def d) st.tree in
      { tree }

    let denote_phi (dmap : Directives.directive_map)
          (p : phi term) (st : env) : env =
      let tree = Tree.map ~f:(BaseInt.denote_phi p) st.tree in
      { tree }

    let denote_jmp (dmap : Directives.directive_map)
          (j : jmp term) (st : env) : env =
      let tree = Tree.map ~f:(BaseInt.denote_jmp j) st.tree in
      { tree }

    let denote_elt (dmap : Directives.directive_map)
          (e : Blk.elt) (st : env) : env =
      if 5 <= TreeEnv.num_leaves st
      then L.warn "env at %a has >=5 splits"
             Tid.ppo @@ Common.elt_to_tid e;
      match e with
      | `Def d -> denote_def dmap d st
      | `Jmp j -> denote_jmp dmap j st
      | `Phi p -> denote_phi dmap p st
  end
end
