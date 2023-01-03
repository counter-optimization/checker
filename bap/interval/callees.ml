open Core_kernel
open Common
open Bap.Std
open Graphlib.Std
open Monads.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

type t

let cls : (t, unit) KB.cls = KB.Class.declare
                               ~package:"uarch-checker"
                               "named-labels"
                               ()
let name = KB.Class.property ~package:"uarch-checker"
             cls
             "name"
             KB.Domain.string

let addr = KB.Class.property ~package:Common.package
             cls
             "addr"
             KB.Domain.string

let names = KB.Class.property ~package:Common.package
             cls
             "label-names"
             Common.string_powset_dom

let interned_name = "collected-named-labels"

(* gets callees one-level deep.
   in the call graph, each node of the graph is a term id (Tid).
   the call graph only includes direct calls.
   calling Getter.get [subroutine] [project] returns a list of
   tids of direct callees and indirect callees.
   this is why an abstract interpretation and previously generated
   absint is required to build this module.
   this only gets callees one-level deep, i.e., if the final callgraph
   contains both direct and indirect callees, then this returns
   a list of tids in the non-transitive closure of the callgraph represented
   as a relation. *)
module Getter(N : NumericDomain) = struct
  module E = struct
    type region = Abstract_memory.Region.t
    type regions = Abstract_memory.Region.Set.t
    type valtypes = Common.cell_t
    include Abstract_memory.Make(N)
  end
  
  module AI = AbstractInterpreter(N)(Abstract_memory.Region)(Abstract_memory.Region.Set)(struct type t = Common.cell_t end)(E)
  module WI = Wrapping_interval
  module SS = Common.SS
  module CG = Graphs.Callgraph 
  
  type t = SS.t

  let name = "get-callees"

  let get_intvl : N.t -> Wrapping_interval.t =
    match N.get Wrapping_interval.key with
    | Some f -> f
    | None -> failwith "Couldn't extract interval information out of product domain"

  (* So this is an address. Tids are not (to me) related to addresses. *)
  let get_callee_of_indirect_exn (exp : exp) (jmp_from : jmp term) (sol : (Tid.t, E.t) Solution.t) : Tid.t =
    let fromtid = Term.tid jmp_from in
    let exp_evaller = AI.denote_exp exp in
    let eval_in_env = Solution.get sol fromtid in
    let (callee, _new_env) = AI.ST.run exp_evaller eval_in_env in
    let callee_as_intvl = get_intvl callee in
    match WI.to_int callee_as_intvl with
    | Some int -> failwith "todo"
    | None ->
       failwith "Couldn't get callee as a single integer in Callees.Getter.get_callee_of_indirect_exn"

  let of_jmp_term_exn (j : jmp term) (sol : (Tid.t, E.t) Solution.t) : Tid.t =
    match Jmp.kind j with
    | Goto (Direct totid) -> totid
    | Goto (Indirect exp) -> get_callee_of_indirect_exn exp j sol
    | Int (interrupt_no, return_tid) ->
       failwith "interrupts not handled in Callee.Getter.of_jmp_term_exn"
    | Call callee ->
       let target = Call.target callee in
       begin
         match target with
         | Direct totid -> totid
         | Indirect exp -> get_callee_of_indirect_exn exp j sol
       end
    | Ret _ -> failwith "returns not handled in Callee.Getter.of_jmp_term_exn" 

  let of_jmp_term_list_exn (js : jmp term list) (sol : (Tid.t, E.t) Solution.t) : Tid.t list =
    List.fold js ~init:[] ~f:(fun tids j ->
        List.cons (of_jmp_term_exn j sol) tids)

  (* here, the edges of CG.t are a list of jmp terms *)
  let get_direct_calls sub proj (sol : (Tid.t, E.t) Solution.t) : Tid.t list =
    let prog = Project.program proj in
    let callgraph : CG.t = Program.to_graph prog in
    let callee_edges = CG.edges callgraph in
    Seq.fold callee_edges
      ~init:[]
      ~f:(fun callees edge ->
        let jmps = CG.Edge.label edge in
        let jmp_to_tids = of_jmp_term_list_exn jmps sol in
        List.append jmp_to_tids callees)
    
  (* returns TID of callees *)
  let get sub proj (sol : (Tid.t, E.t) Solution.t) : t =
    let direct_callees = get_direct_calls sub proj in
    failwith "indirect callees todo"
end

(* for later: https://gitter.im/BinaryAnalysisPlatform/bap?at=5a296116cc1d527f6b0213d1 *)
let run () =
  let open KB.Monad_infix in
  KB.objects T.Program.cls >>= fun objs ->
  Seq.fold
    objs
    ~init:(KB.return ())
    ~f:(fun acc prog ->
      acc >>= fun () ->
      KB.collect T.Label.name prog >>= fun maybe_name ->
      KB.collect T.Label.addr prog >>= fun maybe_addr ->
      match maybe_addr, maybe_name with
      | Some addr, Some labelname ->
         KB.Symbol.intern
        ~package:Common.package
        interned_name
        cls >>= fun putter ->
         let to_add = sprintf "%s:%s" (Bitvec.to_string addr) labelname in
         KB.collect names putter >>= fun cur_names ->
         KB.provide names putter (Set.add cur_names to_add)
      | _, _ -> KB.return ())
  >>= fun () ->
  KB.Symbol.intern
    ~package:Common.package
    interned_name
    cls
