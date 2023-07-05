open Core
open Bap.Std

module Directives(N : Abstract.NumericDomain)
         (E : Abstract.MemoryT with type v = N.t) = struct
  
  module T = struct
    type simple_operand = Var of string | Num of Word.t [@@deriving compare, sexp]
    
    type cnd = Eq of simple_operand * simple_operand
             | Lt of simple_operand * simple_operand
                                        [@@deriving compare, sexp]
    
    type directive = Empty
                   | Value of cnd
                   | Jmp of cnd
                              [@@deriving compare, sexp]

    type t = directive [@@deriving compare, sexp]
  end

  include T
end

module Tree(N : Abstract.NumericDomain)
         (E : Abstract.MemoryT with type v = N.t) = struct
  
  module Directives = Directives(N)(E)
    
  module T = struct
    type node = Leaf of E.t | Parent of {
                    left : node;
                    directive : Directives.directive;
                    right : node;
                  }

    type t = node
  end
  
  include T

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
         (E : Abstract.MemoryT with type v = N.t) = struct

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

module AbsInt = struct
  module Make(N : Abstract.NumericDomain)
           (E : Abstract.MemoryT with type v = N.t
                                  and type region = Common.Region.t
                                  and type regions = Common.Region.Set.t
                                  and type valtypes = Common.cell_t) = struct
    
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
