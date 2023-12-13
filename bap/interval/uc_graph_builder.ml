open Core_kernel
open Bap.Std
open Monads.Std

module BapG = Graphlib.Std
module OcamlG = Graph

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

let log_prefix = sprintf "%s.uc_graph_builder" Common.package
module L = struct
  include Dolog.Log
  let () = set_prefix log_prefix
end

type cedge = (tid * Exp.t option * tid) [@@deriving compare, sexp]

type cedges = cedge list

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

let false_node = Tid.create ()
let false_node_cc = Calling_context.of_tid false_node
  
let false_def =
  let v = Var.create "RAX" (Bil.Types.Imm 64) in
  let exp = Bil.Var v in
  Def.create ~tid:false_node v exp

let () = 
  L.debug "False, top node is at: %a" Tid.ppo false_node;
  L.debug "False, top node has def: %a" Def.ppo false_def

module UcBapG = struct
  type 'a edge = (Calling_context.t * Calling_context.t * 'a)

  type 'a edges = 'a edge list

  let of_cedge (from_, label, to_) =
    (Calling_context.of_tid from_,
     Calling_context.of_tid to_,
     label)
end

module ExpOpt = struct
  type t = Exp.t option [@@deriving compare, sexp]
end

module UcOcamlG = struct
  module MExp = struct
    include Exp
    type t = Exp.t option [@@deriving compare, sexp]
    let default = None
    let s e = Some e
  end

  module T = OcamlG.Imperative.Graph.ConcreteLabeled(Tid)(MExp)

  type edge = T.E.t 

  let of_bapg (type g n e) (module G : BapG.Graph with type t = g and type node = n and type edge = e) (bapg : g) (edge_to_tuple : e -> _ UcBapG.edge) : T.t =
    let cc_to_tid = Calling_context.to_insn_tid in
    let convert_edge (from_, to_, label) : edge =
      T.E.create (cc_to_tid from_) None (cc_to_tid to_)
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

let nonjmpedge from_ to_ = (from_, None, to_)
let jmpedge from_ to_ cnd = (from_, cnd, to_)

let string_of_cedge e = sexp_of_cedge e |> Sexp.to_string_hum
let string_of_bapedge (from_, to_, l) =
  let l = match l with
    | None -> "None"
    | Some exp -> Exp.to_string exp in
  let from_ = Tid.to_string @@ Calling_context.to_insn_tid from_ in
  let to_ = Tid.to_string @@ Calling_context.to_insn_tid to_ in
  sprintf "(%s, %s, %s)" from_ to_ l

(* jmp_never_taken and jmp_always_taken
   only check for unconditionally never or always
   taken jumps based on bools, not trivial expressions
   like: 'jmp to addrA if x == x' *)
let jmp_never_taken cnd =
  match cnd with
  | Bil.Int x ->
    let w = Word.bitwidth x in
    Word.equal x @@ Word.zero w
  | _ -> false

let jmp_always_taken cnd =
  match cnd with
  | Bil.Int x -> 
    let w = Word.bitwidth x in
    Word.equal x @@ Word.one w
  | _ -> false

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

let build_blk_map blks =
  let m : Blk.elt Tid.Map.t = Tid.Map.empty in
  Seq.fold blks ~init:m ~f:(fun m b ->
    let blktid = Term.tid b in
    match Seq.hd @@ Blk.elts b with
    | Some e -> Tid.Map.set m ~key:blktid ~data:e
    | None -> m)

let build_tid_map blks =
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

let rec build_jmp_edge ?(interproc = false) ?(prevcnd = None) blkmap tidtoeltsmap proj from_ jmp : cedge list =
  let find = Tid.Map.find in
  let cnd = Jmp.cond jmp in
  let jmptarget t cnd from_ = match find blkmap t with
    | Some (`Jmp _) -> begin match find tidtoeltsmap t with
      | Some elts -> (* then elts is a seq of jmp terms only *)
        Seq.to_list elts 
        |> List.fold ~init:([], cnd, true)
             ~f:(fun ((es, cnds, continue) as acc) -> function
               | `Jmp j ->
                 if not continue
                 then acc
                 else
                   let nextcnd = Jmp.cond j in
                   let nextjmps = build_jmp_edge ~interproc ~prevcnd:cnds blkmap tidtoeltsmap proj from_ j in
                   let cnds_so_far = match cnds with
                     | Some cs -> Bil.BinOp (Bil.AND, cs, (Bil.UnOp (Bil.NOT, nextcnd)))
                     | None -> Bil.UnOp (Bil.NOT, nextcnd) in
                   (nextjmps :: es, Some cnds_so_far, not @@ jmp_always_taken nextcnd)
               | _ -> failwith "malformed function in handling fallthrough jumps")
        |> (fun (a, _, _) -> a)
        |> List.join
      | None -> []
      end
    | Some (`Def d) -> [(from_, cnd, Term.tid d)]
    | Some (`Phi p) -> raise @@ BadCfg "ssa not allowed"
    | None -> raise @@ BadCfg "jmp to empty bb"
  in
  let get_ret_edge = function
    | DRetTo t -> jmptarget t None false_node
    | IRetTo e -> raise @@ BadCfg "Indirect rets not handled"
    | NoRet -> []
  in
  if jmp_never_taken cnd
  then []
  else
    let always_taken = jmp_always_taken cnd in
    let cnd = match prevcnd with
      | Some prevcnd -> Some (Bil.BinOp (Bil.AND, prevcnd, cnd))
      | None when always_taken -> None
      | None -> Some cnd in
    match jmp_type jmp with
    | DJmp t -> jmptarget t cnd from_
    | IJmp e -> [(from_, cnd, false_node)]
    | DCall (t, retto) ->
      if interproc
      then [] (* todo *)
      else get_ret_edge retto @ [(from_, cnd, false_node)]
    | ICall (e, retto) ->
      get_ret_edge retto @ [(from_, cnd, false_node)]
    | DRet t -> [] (* todo *)
    | IRet e -> [] (* todo *)
    | Interrupt -> []

let rec of_defs
          ?(idxst : Idx_calculator.t option = None)
          ?(total = []) = function
  | f :: (s :: xs as nxt) ->
    let fst = Term.tid f in
    let snd = Term.tid s in
    begin
      match idxst with
      | Some idxst ->
        if Idx_calculator.is_part_of_idx_insn idxst fst
        then of_defs ~total nxt
        else if Idx_calculator.is_part_of_idx_insn idxst snd
        then of_defs ~total (f :: xs)
        else of_defs ~total:(nonjmpedge fst snd :: total) nxt
      | None -> of_defs ~total:(nonjmpedge fst snd :: total) nxt
    end
  | _ -> total

let of_blk ?(idxst : Idx_calculator.t option = None) ?(interproc = false) blkmap tidtoeltsmap proj b =
  let defs = Term.enum def_t b |> Seq.to_list in
  let def_edges = of_defs ~idxst defs in
  let last_def = List.last defs in
  (* last_def is None <=> def_edges is [] <=> the basic block is empty of defs *)
  let rec build_jmps ?(es = []) dt = function
    | [] -> es
    | j :: js ->
      let es = build_jmp_edge blkmap tidtoeltsmap proj dt j @ es in
      let cnd = Jmp.cond j in
      if jmp_always_taken cnd
      then es
      else build_jmps ~es dt js
  in
  match last_def with
  | None -> []
  | Some d ->
    let dt = Term.tid d in
    let jmps = Term.enum jmp_t b |> Seq.to_list in
    build_jmps dt jmps @ def_edges

module IntraNoResolve : sig
  val of_sub : ?idxst:Idx_calculator.t option -> Project.t -> sub term -> cedge list * Blk.elt Tid.Map.t
  val of_sub_to_bapedges : ?idxst:Idx_calculator.t option -> Project.t -> sub term -> ExpOpt.t UcBapG.edges * Blk.elt Tid.Map.t
end = struct
  let log_prefix = sprintf "%s.IntraNoResolve" log_prefix
  module L = struct
    include Dolog.Log
    let () = set_prefix log_prefix
  end

  let of_sub ?(idxst : Idx_calculator.t option = None) proj s =
    let blks = Term.enum blk_t s in
    let blkmap = build_blk_map blks in
    let tidmap = build_tid_map blks
                 |> Tid.Map.set ~key:false_node ~data:(`Def false_def) in
    let tidtoeltsmap = build_tidtoblkelts_map blks in
    let edges = Seq.map blks ~f:(of_blk ~idxst blkmap tidtoeltsmap proj)
                |> Seq.to_list
                |> List.join in
    edges, tidmap

  let of_sub_to_bapedges ?(idxst : Idx_calculator.t option = None) proj s =
    let edges, tidmap = of_sub ~idxst proj s in
    List.map edges ~f:UcBapG.of_cedge, tidmap
end

module Inter : sig
  type t
    
  val of_sub : ?idxst:Idx_calculator.t option -> Project.t -> sub term -> unit
end = struct
  
  type t = {
    interedges : cedge list;
    edges : cedge list;
    callrets : cedge list;
    subs : string list;
    tidmap : Blk.elt Tid.Map.t
  }
    
  let log_prefix = sprintf "%s.Inter" log_prefix
  module L = struct
    include Dolog.Log
    let () = set_prefix log_prefix
  end

  let emp : t = {
    interedges = [];
    edges = [];
    callrets = [];
    subs = [];
    tidmap = Tid.Map.empty;
  }

  let edges_for_iter st = st.interedges @ st.edges
  
  let of_sub ?(idxst : Idx_calculator.t option = None) proj s = ()
end
