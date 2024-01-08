open Core_kernel
open Bap.Std
open Monads.Std

module BapG = Graphlib.Std
module OcamlG = Graph

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

module L = struct
  include Dolog.Log
  let log_prefix = sprintf "%s.uc_graph_builder" Common.package
  let () = set_prefix log_prefix
end

type cedge = (tid * Exp.t option * tid) [@@deriving compare, sexp]

type cedges = cedge list [@@deriving compare, sexp]

type ucret_kind = DRetTo of tid | IRetTo of Exp.t | NoRet
                  [@@deriving compare, sexp]

type ucjmp_kind = DCall of tid * ucret_kind
                | ICall of Exp.t * ucret_kind
                | DJmp of tid
                | IJmp of Exp.t
                | DRet of tid
                | IRet of Exp.t
                | Interrupt
[@@deriving compare, sexp]

type exn += BadCfg of string

(* TODO: ABI composition *)   
let false_node = Tid.create ()
let false_def =
  let v = Var.create "RAX" (Bil.Types.Imm 64) in
  let exp = Bil.Var v in
  Def.create ~tid:false_node v exp
    
let havoc_cnd = Bil.Var (Var.create ~fresh:true "HAVOC" (Bil.Types.Imm 64))
let is_havoc_cnd v = Var.name v |> String.equal "HAVOC"

let () = 
  L.debug "False, top node is at: %a" Tid.ppo false_node;
  L.debug "False, top node has def: %a" Def.ppo false_def

(* TODO: ABI composition *)   
let entrytid : Tid.t = Tid.create () 
let entry : def term =
  let v = Var.create "RDI" (Bil.Types.Imm 64) in
  let exp = Bil.Var v in
  Def.create ~tid:entrytid v exp
let entry_blk_elt : Blk.elt = Blk.(`Def entry)

(* TODO: ABI composition *)   
let exittid : Tid.t = Tid.create ()
let exit : def term =
  let v = Var.create "RAX" (Bil.Types.Imm 64) in
  let exp = Bil.Var v in
  Def.create ~tid:exittid v exp
let exit_blk_elt : Blk.elt = Blk.(`Def exit)

(* jmp_never_taken and jmp_always_taken
   only check for unconditionally never or always
   taken jumps based on bools, not trivial expressions
   like: 'jmp to addrA if x == x' *)
let jmp_never_taken (cnd : Bil.exp) : bool =
  let is_zero (x : Word.t) : bool =
    Word.equal x @@ Word.zero @@ Word.bitwidth x
  in
  match cnd with
  | Bil.Int x when is_zero x -> true
  | _ -> false

let jmp_always_taken (cnd : Bil.exp) : bool =
  let is_one (x : Word.t) : bool =
    Word.equal x @@ Word.one @@ Word.bitwidth x
  in
  match cnd with
  | Bil.Int x when is_one x -> true
  | _ -> false

module ExpOpt = struct
  type t = Exp.t option [@@deriving compare, sexp, equal]

  let add_cnd ~(start : t) ~(add : t) : t =
    match start with
    | None -> add
    | Some start_cnd when jmp_always_taken start_cnd -> add
    | Some start_cnd when jmp_never_taken start_cnd -> start
    | Some start_cnd -> match add with
      | None -> start
      | Some add_cnd when jmp_always_taken add_cnd -> start
      | Some add_cnd when jmp_never_taken add_cnd -> add
      | Some add_cnd -> Some (Bil.BinOp (Bil.AND, start_cnd, add_cnd))
end

module UcBapG = struct
  type edge = (Tid.t * Tid.t * ExpOpt.t) [@@deriving sexp, compare, equal]

  type edges = edge list [@@deriving sexp, compare, equal]

  let of_cedge (from_, label, to_) : edge = (from_, to_, label)

  let create ~(from_ : Tid.t) ~(to_ : Tid.t) : edge =
    (from_, to_, None)
end

module UcOcamlG = struct
  module MExp = struct
    include Exp
    type t = Exp.t option [@@deriving compare, sexp, equal]
    let default = None
    let s e = Some e
  end

  module T = OcamlG.Imperative.Graph.ConcreteLabeled(Tid)(MExp)

  type edge = T.E.t 

  let of_bapg (type g n e) (module G : BapG.Graph with type t = g and type node = n and type edge = e) (bapg : g) (edge_to_tuple : e -> UcBapG.edge) : T.t =
    let convert_edge (from_, to_, label) : edge =
      T.E.create from_ None to_
    in
    let add_edge = T.add_edge_e in
    let nnodes = G.number_of_nodes bapg in
    let g = T.create ~size:nnodes () in
    let update_edges es = 
      Seq.iter es ~f:(fun e ->
        add_edge g @@ convert_edge @@ edge_to_tuple e);
    in
    let edges = G.edges bapg in
    update_edges edges;
    g
end

module OcamlGraphWriter = struct
  module T = struct 
    include UcOcamlG.T

    let tid_to_vertex_name t =
      let tid_str = Tid.to_string t
                    |> String.filter ~f:(function
                      | '%' -> false
                      | _ -> true) in
      "node_" ^ tid_str

    let edge_attributes _ = []

    let default_edge_attributes _ = []

    let get_subgraph _ = None

    let vertex_attributes _ = []
      (* let tids = Tid.to_string n in *)
      (* OcamlG.Graphviz.DotAttributes.([`Label tids]) *)

    let vertex_name = tid_to_vertex_name

    let default_vertex_attributes _ = []

    let graph_attributes _ = []
  end

  include OcamlG.Graphviz.Dot(T)
end

let nonjmpedge from_ to_ = (from_, to_, None)
let jmpedge from_ to_ cnd = (from_, to_, cnd)

let string_of_cedge e = sexp_of_cedge e |> Sexp.to_string_hum
let string_of_bapedge (from_, to_, l) =
  let l = match l with
    | None -> "None"
    | Some exp -> Exp.to_string exp in
  let from_ = Tid.to_string from_ in
  let to_ = Tid.to_string to_ in
  sprintf "(%s, %s, %s)" from_ to_ l

let jmp_type j =
  match Jmp.kind j with
  | Call c ->
    let retto = match Call.return c with
      | Some (Direct t) -> DRetTo t
      | Some (Indirect e) -> IRetTo e
      | None -> NoRet in
    begin match Call.target c with
    | Direct t -> DCall (t, retto)
    | Indirect e -> ICall (e, retto)
    end
  | Goto (Direct t) -> DJmp t
  | Goto (Indirect e) -> IJmp e
  | Ret (Direct t) -> DRet t
  | Ret (Indirect e) -> IRet e
  | Int (_, _) -> Interrupt

let build_blk_map (blks : blk term Seq.t) : Blk.elt Tid.Map.t =
  let m : Blk.elt Tid.Map.t = Tid.Map.empty in
  Seq.fold blks ~init:m ~f:(fun m b ->
    let blktid = Term.tid b in
    match Seq.hd @@ Blk.elts b with
    | Some e -> Tid.Map.set m ~key:blktid ~data:e
    | _ -> m)

let build_tid_map (blks : blk term Seq.t) : Blk.elt Tid.Map.t =
  let m = Tid.Map.empty in
  Seq.fold blks ~init:m ~f:(fun m b ->
    Seq.fold (Blk.elts b) ~init:m ~f:(fun m e ->
      let t = Common.elt_to_tid e in
      Tid.Map.set m ~key:t ~data:e))

let build_tidtoblkelts_map blks =
  let m : Blk.elt Seq.t Tid.Map.t = Tid.Map.empty in
  Seq.fold blks ~init:m ~f:(fun m b ->
    let blktid = Term.tid b in
    let elts = Blk.elts b in 
    Tid.Map.set m ~key:blktid ~data:elts)

let rec build_jmp_edge
          ?(prevcnd = None)
          (blkmap : Blk.elt Tid.Map.t)
          (tidtoeltsmap : Blk.elt Seq.t Tid.Map.t)
          (from_ : tid)
          (jmp : jmp term) : UcBapG.edge list =
  let find = Tid.Map.find in
  (* let rec dfs_jmp_trace ~(target : Tid.t) *)
  (*           ~(from_ : Tid.t) *)
  (*           ~(cnd : ExpOpt.t) : UcBapG.edges = *)
  (*   match find blkmap target with *)
  (*   | Some (`Def d) -> [(from_, Term.tid d, cnd)] *)
  (*   | Some (`Phi p) -> raise (BadCfg "phi not supported") *)
  (*   | Some (`Jmp j) -> *)
  (*     let target_cnd = Jmp.cond j in *)
  (*   | None -> raise (BadCfg "jump to empty block/tidmap not filled out") *)
  (* in *)
  let jmptarget (target : Tid.t)
        (cnd : ExpOpt.t)
        (from_ : Tid.t) : UcBapG.edges =
    match find blkmap target with
    | Some (`Jmp _) ->
      begin match find tidtoeltsmap target with
      | Some elts ->
        (* then elts is a seq of jmp terms only *)
        Seq.to_list elts 
        |> List.fold ~init:([], cnd, true)
             ~f:(fun ((es, cnds, continue) as acc) -> function
               | `Jmp j ->
                 if not continue
                 then acc
                 else
                   let nextcnd = Jmp.cond j in
                   let nextjmps = build_jmp_edge ~prevcnd:cnds blkmap tidtoeltsmap from_ j in
                   let not_nextcnd = Bil.UnOp (Bil.NOT, nextcnd) in
                   let cnds_so_far = ExpOpt.add_cnd ~start:cnds ~add:(Some not_nextcnd) in
                   (* let cnds_so_far = match cnds with *)
                   (*   | Some cs -> Bil.BinOp (Bil.AND, cs, (Bil.UnOp (Bil.NOT, nextcnd))) *)
                   (*   | None -> Bil.UnOp (Bil.NOT, nextcnd) *)
                   (* in *)
                   (nextjmps :: es, cnds_so_far, not @@ jmp_always_taken nextcnd)
               | _ -> failwith "malformed function in handling fallthrough jumps")
        |> (fun (a, _, _) -> a)
        |> List.join
      | None -> []
      end
    | Some (`Def d) -> [(from_, Term.tid d, cnd)]
    | Some (`Phi p) -> raise @@ BadCfg "ssa not allowed"
    | None -> raise (BadCfg "jmp to empty bb")
  in
  let get_ret_edge : ucret_kind -> UcBapG.edges = function
    | DRetTo t -> jmptarget t (Some havoc_cnd) from_
    | IRetTo e -> raise (BadCfg "Indirect rets not handled")
    | NoRet -> []
  in
  let cnd = Jmp.cond jmp in
  if jmp_never_taken cnd
  then []
  else
    let cnd = ExpOpt.add_cnd ~start:prevcnd ~add:(Some cnd) in
    (* let cnd = match prevcnd with *)
    (*   | Some prevcnd -> Some (Bil.BinOp (Bil.AND, prevcnd, cnd)) *)
    (*   | None when jmp_always_taken cnd -> None *)
    (*   | None -> Some cnd *)
    (* in *)
    match jmp_type jmp with
    | DJmp t -> jmptarget t cnd from_
    | IJmp e -> [(from_, false_node, cnd)]
    | DCall (t, retto) ->
      (* get_ret_edge retto  *)
      (* get_ret_edge retto @ [(from_, false_node, cnd)] *)
      get_ret_edge retto @ [(from_, Term.tid jmp, None)]
    | ICall (e, retto) -> (* get_ret_edge retto *)
    get_ret_edge retto @ [(from_, false_node, cnd)]
    | DRet t -> [] (* todo *)
    | IRet e -> [] (* todo *)
    | Interrupt -> []

let rec of_defs ?(idxst : Idx_calculator.t option = None)
          ?(total = []) : def term list -> UcBapG.edge list =
  function
  | f :: (s :: xs as nxt) ->
    let fst = Term.tid f in
    let snd = Term.tid s in
    of_defs ~total:(nonjmpedge fst snd :: total) nxt
  | _ -> total

let of_blk ?(idxst : Idx_calculator.t option = None)
      (blkmap : Blk.elt Tid.Map.t)
      (tidtoeltsmap : Blk.elt Seq.t Tid.Map.t)
      (proj : Project.t)
      (b : blk term) : UcBapG.edge list =
  let get_jmps (blk : Blk.t) : jmp term list =
    (* rev:true ==> jmps first in reverse order *)
    Blk.elts ~rev:true blk
    |> Seq.fold
         ~init:[]
         ~f:(fun jmps -> function
           | `Jmp j -> j :: jmps
           | _ -> jmps)
  in
  let rec build_jmps ?(es : UcBapG.edges = [])
            ?(prev_cnds : ExpOpt.t)
            (dt : Tid.t) : jmp term list -> UcBapG.edges =
    function
    | [] -> es
    | j :: js ->
      let es' = (build_jmp_edge blkmap tidtoeltsmap dt j) @ es in
      let cnd = Jmp.cond j in
      if jmp_always_taken cnd
      then es'
      else if jmp_never_taken cnd
      then build_jmps ~es dt js
      else build_jmps ~es:es' dt js
  in
  let defs = Term.enum def_t b |> Seq.to_list in
  let def_edges = of_defs ~idxst defs in
  let last_def = List.last defs in
  (* last_def is None <=> def_edges is [] <=> the basic block is empty of defs *)
  match last_def with
  | None -> []
  | Some d ->
    let dt = Term.tid d in
    let jmps = get_jmps b in
    build_jmps dt jmps @ def_edges

type edges = (Tid.t * Tid.t * ExpOpt.t) list

module TidAndCond = struct
  type t = Tid.t * ExpOpt.t
  [@@deriving compare, sexp, equal]
end

module TCCmp = struct
  include TidAndCond
  include Comparable.Make(TidAndCond)
end

module TCSet = struct
  include Set.Make_using_comparator(TCCmp)
end
                               
(* 
 * a node has 0 or more succesors and 0 or more predecessors
 *)                               
let remove_dead_defs (edges : edges)
      (dead : Tid.Set.t) : edges =
  let m_succs = Tid.Map.empty in
  let m_preds = Tid.Map.empty in
  let add_node (tm : TCSet.t Tid.Map.t)
        (key : tid)
        (data : tid)
        (cnd : ExpOpt.t) : TCSet.t Tid.Map.t =
    Tid.Map.update tm key ~f:(function
      | Some prev -> TCSet.add prev (data, cnd)
      | None -> TCSet.singleton (data, cnd))
  in
  let remove_node (tm : TCSet.t Tid.Map.t)
        (key : tid) ~(other : tid) : TCSet.t Tid.Map.t =
    Tid.Map.update tm key ~f:(function
      | Some prev -> TCSet.filter prev ~f:(fun (t,_) ->
        not @@ Tid.equal t other)
      | None -> TCSet.empty)
  in
  let add_succs_and_preds ~m_succs ~m_preds edges =
    List.fold edges
      ~init:(m_succs, m_preds)
      ~f:(fun (m_succs, m_preds) (from_, to_, cnd) ->
        (add_node m_succs from_ to_ cnd,
         add_node m_preds to_ from_ cnd))
  in
  let find (tm : TCSet.t Tid.Map.t)
        (n : tid) : TidAndCond.t list =
    Tid.Map.find tm n
    |> Option.value ~default:TCSet.empty
    |> TCSet.to_list
  in
  let combine (c1 : ExpOpt.t) (c2 : ExpOpt.t) : ExpOpt.t =
    match c1, c2 with
    | None, None -> None
    | None, Some x -> Some x
    | Some x, None -> Some x
    | Some x, Some y -> Some (Bil.BinOp (Bil.AND, x, y))
  in
  let rec cart_prod ~(res : edges)
            ~(succs : TidAndCond.t list)
            ~(preds : TidAndCond.t list) : edges =
    if List.is_empty succs || List.is_empty preds
    then res
    else match preds with
    | [] -> []
    | (predtid, predcnd) :: rst ->
      let new_edges =
        List.map succs
          ~f:(fun (succtid,succcnd) ->
            (predtid, succtid, combine predcnd succcnd))
      in
      cart_prod ~res:(new_edges @ res) ~succs ~preds:rst
  in
  let new_edges (n : tid)
        (m_succs : TCSet.t Tid.Map.t)
        (m_preds : TCSet.t Tid.Map.t)
    : (edges * TCSet.t Tid.Map.t * TCSet.t Tid.Map.t) =
    let succs = find m_succs n in
    let preds = find m_preds n in
    let edges = cart_prod ~res:[] ~succs ~preds in
    (* remove node n from m_succs and m_preds *)
    let m_succs = Tid.Map.remove m_succs n in
    let m_preds = Tid.Map.remove m_preds n in
    (* update succs and preds with new edge info *)
    let m_succs, m_preds = add_succs_and_preds edges
                             ~m_succs ~m_preds
    in
    (* remove old succ and pred info of n in m_succs and m_preds *)
    let m_succs, m_preds =
      List.fold edges
        ~init:(m_succs, m_preds)
        ~f:(fun (m_succs, m_preds) (from_, to_, _) ->
          (remove_node m_succs from_ ~other:n,
           remove_node m_preds to_ ~other:n))
    in
    (edges, m_succs, m_preds)
  in
  let remove_edges (n : tid) : edges -> edges =
    List.filter ~f:(fun (from_, to_, _) ->
      not (Tid.equal from_ n || Tid.equal to_ n))
  in
  let m_succs, m_preds = add_succs_and_preds edges
                           ~m_succs ~m_preds
  in
  let edges, _m_succs, _m_preds =
    Tid.Set.fold dead
      ~init:(edges, m_succs, m_preds)
      ~f:(fun (edges, m_succs, m_preds) deadtid ->
        let edges', m_succs, m_preds =
          new_edges deadtid m_succs m_preds
        in
        edges' @ remove_edges deadtid edges, m_succs, m_preds)
  in
  edges

module IntraNoResolve : sig
  val of_sub : ?idxst:Idx_calculator.t option -> Project.t -> sub term -> UcBapG.edges * Blk.elt Tid.Map.t
end = struct
  
  module L = struct
    include Dolog.Log
    let log_prefix = sprintf "%s.IntraNoResolve" L.log_prefix
    let () = set_prefix log_prefix
  end

  let of_sub ?(idxst : Idx_calculator.t option = None) proj s =
    let blks = Term.enum blk_t s in
    let blkmap = build_blk_map blks in
    let tidmap = build_tid_map blks
                 |> Tid.Map.set
                      ~key:false_node
                      ~data:(`Def false_def)
                 |> Tid.Map.set
                      ~key:entrytid
                      ~data:entry_blk_elt
                 |> Tid.Map.set
                      ~key:exittid
                      ~data:exit_blk_elt
    in
    let tidtoeltsmap = build_tidtoblkelts_map blks in
    let edges = Seq.map blks
                  ~f:(of_blk ~idxst blkmap tidtoeltsmap proj)
                |> Seq.to_list
                |> List.join
    in
    let edges = match idxst with
      | None -> edges
      | Some idxst ->
        let idx_insn_tids = Tid.Map.fold tidmap
                              ~init:Tid.Set.empty
                              ~f:(fun ~key ~data tids ->
                                if Idx_calculator.is_part_of_idx_insn idxst key
                                then Tid.Set.add tids key
                                else tids)
        in
        let edges = remove_dead_defs edges idx_insn_tids in
        edges
    in
    edges, tidmap
end
