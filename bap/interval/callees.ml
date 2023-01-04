open Core_kernel
open Common
open Bap.Std
open Graphlib.Std
open Monads.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

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

  open Or_error.Monad_infix
  
  type t = Tid.t list

  let name = "get-callees"

  let get_intvl : N.t -> Wrapping_interval.t =
    match N.get Wrapping_interval.key with
    | Some f -> f
    | None -> failwith "callees.get_intvl: Couldn't extract interval information out of product domain"

  let get_bb_tid_sub_db (prog : Program.t) : (Tid.t, Tid.t) List.Assoc.t =
    let blk_tids_of_sub (s : sub term) : Tid.t list =
      Term.enum blk_t s
      |> Seq.to_list
      |> List.map ~f:(Term.tid)
    in
    let db_of_one_sub (s : sub term) : (Tid.t, Tid.t) List.Assoc.t =
      let sub_tid = Term.tid s in
      blk_tids_of_sub s
      |> List.map ~f:(fun blk_tid -> (blk_tid, sub_tid))
    in
    Term.enum sub_t prog
    |> Seq.to_list
    |> List.map ~f:db_of_one_sub
    |> List.join

  (* So this is an address. According to gitter chat with benjamin and ivan,
     using normal disassembly from the project init will leave tids and labels
     as corresponding to the address (so both should be equal).
   *)
  let get_callee_of_indirect (exp : exp) (jmp_from : jmp term) (prog : Program.t) (sol : (Tid.t, E.t) Solution.t) : Tid.t Or_error.t =
    let fromtid = Term.tid jmp_from in
    let exp_evaller = AI.denote_exp exp in
    let eval_in_env = Solution.get sol fromtid in
    let (callee, _new_env) = AI.ST.run exp_evaller eval_in_env in
    let callee_as_intvl = get_intvl callee in
    match WI.to_int callee_as_intvl with
    | Some addr_int ->
       let callee_addr = addr_int |> Addr.of_int ~width:64 in
       let () = printf "get_callee_of_indirect: callee_addr as addr is %a\n"
                  Addr.ppo callee_addr
       in
       let () = printf "get_callee_of_indirect: callee_addr as Wi is %s\n"
                @@ WI.to_string callee_as_intvl
       in
       let callee_tid = Tid.for_addr callee_addr in
       let () = printf "get_callee_of_indirect: callee_addr as tid is %a\n"
                  Tid.ppo callee_tid
       in
       let bb_tid_sub_db = get_bb_tid_sub_db prog in
       (match List.Assoc.find ~equal:Tid.equal bb_tid_sub_db callee_tid with
        | Some sub_tid -> Ok sub_tid
        | None -> Or_error.error_string
                    "get_callee_of_indirect_exn: Couldn't find callee sub tid for that addr")
    | None ->
       Or_error.error_string
         "get_callee_of_indirect_exn: Couldn't get callee as a single integer in Callees.Getter.get_callee_of_indirect_exn"

  let of_jmp_term (j : jmp term) (prog : Program.t) (sol : (Tid.t, E.t) Solution.t) : Tid.t Or_error.t =
    match Jmp.kind j with
    | Goto (Direct totid) -> Ok totid
    | Goto (Indirect exp) -> get_callee_of_indirect exp j prog sol
    | Int (interrupt_no, return_tid) ->
       Or_error.error_string
         "interrupts not handled in Callee.Getter.of_jmp_term_exn"
    | Call callee ->
       let target = Call.target callee in
       begin
         match target with
         | Direct totid -> Ok totid
         | Indirect exp -> get_callee_of_indirect exp j prog sol
       end
    | Ret _ -> Or_error.error_string
                 "of_jmp_term_exn: returns not handled in Callee.Getter.of_jmp_term_exn" 

  let of_jmp_term_list (js : jmp term list) (prog : Program.t) (sol : (Tid.t, E.t) Solution.t) : Tid.t list Or_error.t =
    List.fold js ~init:(Ok []) ~f:(fun tids j ->
        tids >>= fun tids ->
        let new_tid = of_jmp_term j prog sol in
        new_tid >>= fun new_tid ->
        Ok (List.cons new_tid tids))

  (* here, the edges of CG.t are a list of jmp terms *)
  let get_direct_calls sub proj (sol : (Tid.t, E.t) Solution.t) : Tid.t list Or_error.t =
    let prog = Project.program proj in
    let callgraph : CG.t = Program.to_graph prog in
    let callee_edges = CG.edges callgraph in
    Seq.fold callee_edges
      ~init:(Ok [])
      ~f:(fun callees edge ->
        let jmps = CG.Edge.label edge in
        callees >>= fun callees ->
        let jmp_to_tids = of_jmp_term_list jmps prog sol in
        jmp_to_tids >>= fun jmp_to_tids ->
        Ok (List.append jmp_to_tids callees))

  let get_all_jmps (sub : sub term) : jmp term list =
    let blks = Term.enum blk_t sub in
    let jmps = Seq.map blks ~f:(fun b -> Term.enum jmp_t b |> Seq.to_list) in
    let jmps_list = Seq.to_list jmps in
    List.join jmps_list

  let get_indirect_calls (sub : sub term) (proj : Project.t)
        (sol : (Tid.t, E.t) Solution.t)
      : Tid.t list Or_error.t =
    let is_indirect_call (j : jmp term) : bool =
      match Jmp.kind j with
      | Call target ->
         let target_label = Call.target target in
         (match target_label with
          | Direct _ -> false
          | Indirect _ -> true)
      | Goto _
      | Ret _ 
      | Int (_, _) -> false
    in
    let prog = Project.program proj in
    let all_jmps = get_all_jmps sub in
    let indirect_jmps = List.filter all_jmps ~f:is_indirect_call
    in
    List.fold indirect_jmps ~init:(Ok [])
      ~f:(fun tids jmp ->
        tids >>= fun tids ->
        of_jmp_term jmp prog sol >>= fun callee_tid ->
        Ok (List.cons callee_tid tids))

  let sub_of_tid_for_prog (p : Program.t) (t : Tid.t) : sub term Or_error.t =
    match Term.find sub_t p t with
        | Some callee_sub -> Ok callee_sub
        | None ->
           Or_error.error_string @@
             Format.sprintf "Couldn't find callee sub for tid %a" Tid.pps t
    
  (* returns list of sub term of callees *)
  let get (sub : sub term) (proj : Project.t)
        (sol : (Tid.t, E.t) Solution.t)
      : sub term list Or_error.t =
    let prog = Project.program proj in
    get_direct_calls sub proj sol >>= fun direct_callees ->
    get_indirect_calls sub proj sol >>= fun indirect_callees ->
    let all_callees = List.append indirect_callees direct_callees in
    List.fold all_callees ~init:(Ok [])
      ~f:(fun subs tid ->
        subs >>= fun subs ->
        sub_of_tid_for_prog prog tid >>= fun newsub ->
        Ok (List.cons newsub subs))
end