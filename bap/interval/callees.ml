open Core_kernel
open Common
open Bap.Std
open Graphlib.Std
open Monads.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

open Abstract

let src = Uc_log.create_src "callees"

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
    type region = Region.t
    type regions = Region.Set.t
    type valtypes = Common.cell_t
    include Abstract_memory.Make(N)
  end

  module WI = Wrapping_interval
  module SS = Common.SS
  module CG = Graphs.Callgraph

  open Or_error.Monad_infix

  type rel = Common.CalleeRel.t
  type t = rel list

  let name = "get-callees"

  let get_intvl : N.t -> Wrapping_interval.t =
    match N.get Wrapping_interval.key with
    | Some f -> f
    | None -> failwith "callees.get_intvl: Couldn't extract interval information out of product domain"

  let sub_name_of_tid_exn (proj : Project.t) (tid : Tid.t) : string =
    let prog = Project.program proj in
    match Term.find sub_t prog tid with
    | Some sub -> Sub.name sub
    | None -> failwith @@
      Format.sprintf 
        "in sub_name_of_tid_exn, couldn't find sub with tid %a" Tid.pps tid

  let get_bb_tid_sub_db (prog : Program.t) : (Tid.t, Tid.t) List.Assoc.t =
    let blk_tids_of_sub (s : sub term) : Tid.t list =
      Term.enum blk_t s
      |> Seq.to_list
      |> List.map ~f:(Term.tid) in
    let db_of_one_sub (s : sub term) : (Tid.t, Tid.t) List.Assoc.t =
      let sub_tid = Term.tid s in
      blk_tids_of_sub s
      |> List.map ~f:(fun blk_tid -> (blk_tid, sub_tid)) in
    Term.enum sub_t prog
    |> Seq.to_list
    |> List.map ~f:db_of_one_sub
    |> List.join

  (* So this is an address. According to gitter chat with benjamin and ivan,
     using normal disassembly from the project init will leave tids and labels
     as corresponding to the address (so both should be equal). *)
  let get_callee_of_indirect (exp : exp) (jmp_from : jmp term) (prog : Program.t) (sub : sub term) sol : rel Or_error.t =
    let () = printf "in get_callee_of_indirect\n%!" in
    let fromtid = Term.tid jmp_from in
    let callee = sol fromtid exp
                 |> List.remove_consecutive_duplicates ~equal:N.equal
    in
    let callee = if 1 <> List.length callee
      then
        failwith "[Callees] failed to use tracepartabsint to get callee of indirect"
      else
        List.hd_exn callee
    in

    let callee_as_intvl = get_intvl callee in
    let points_to_only_one_addr = Wrapping_interval.is_const callee_as_intvl in
    if not points_to_only_one_addr
    then
      Or_error.error_string @@
      Format.sprintf
        "in get_callee_of_indirect, jmp indirect exp %a points to more than one location (%s)"
        Jmp.pps jmp_from
        (Wrapping_interval.to_string callee_as_intvl)
    else
      match WI.to_int callee_as_intvl with
      | Ok addr_int ->
        let callee_addr = addr_int |> Addr.of_int ~width:64 in
        let callee_tid = Tid.for_addr callee_addr in
        let bb_tid_sub_db = get_bb_tid_sub_db prog in
        (match List.Assoc.find ~equal:Tid.equal bb_tid_sub_db callee_tid with
         | Some sub_tid ->
           let caller_tid = Term.tid sub in
           Ok { callee = sub_tid ;
                caller = caller_tid ;
                callsite = fromtid }
         | None -> Or_error.error_string @@
           sprintf
             "get_callee_of_indirect_exn: Couldn't find callee sub tid for that addr : callee_tid is %a, jmp_from tid is %a, callee_addr is %a"
             Tid.pps callee_tid Tid.pps (Term.tid jmp_from) Bitvector.pps callee_addr)
      | Error e ->
        let wi_err_msg = Error.to_string_hum e in
        let err_msg =
          sprintf "get_callee_of_indirect_exn: Couldn't get callee as a single integer: %s"
            wi_err_msg in
        Or_error.error_string err_msg

  let of_jmp_term (j : jmp term) (sub : sub term) (prog : Program.t) sol : rel option Or_error.t =
    let open Or_error.Monad_infix in
    let is_ret j =
      let sema = Term.get_attr j Disasm.insn in
      match sema with
      | Some sema -> Insn.is Insn.return sema
      | None -> false
    in
    let caller_tid = Term.tid sub in
    let callsite_tid = Term.tid j in
    match Jmp.kind j with
    | _ when is_ret j -> Ok None
    (* for now, direct jmps not considered a direct or indirect call *)
    | Goto (Direct totid) -> Ok None
    | Goto (Indirect exp) ->
      get_callee_of_indirect exp j prog sub sol >>= fun callee ->
      Ok (Some callee)
    | Int (interrupt_no, return_tid) ->
      Or_error.error_string
        "interrupts not handled in Callee.Getter.of_jmp_term_exn"
    | Call callee ->
      let target = Call.target callee in
      (match target with
       | Direct totid -> Ok (Some { caller = caller_tid ;
                                    callee = totid ;
                                    callsite = callsite_tid })
       | Indirect exp ->
         get_callee_of_indirect exp j prog sub sol >>= fun callee ->
         Ok (Some callee))
    | Ret _ ->
      Or_error.error_string
        "of_jmp_term_exn: returns not handled in Callee.Getter.of_jmp_term_exn"

  let get_callees sub proj sol : rel Or_error.t list =
    let prog = Project.program proj in
    let blks = Term.enum blk_t sub in
    let jmp_terms = Seq.map blks ~f:(fun b -> Term.enum jmp_t b |> Seq.to_list)
                    |> Seq.to_list
                    |> List.join in
    let callees = List.map jmp_terms ~f:(fun jt ->
      of_jmp_term jt sub prog sol) in
    List.filter callees ~f:(function
      | Ok maybe_cr -> Option.is_some maybe_cr
      | Error _ -> true)
    |> List.map ~f:(function
      | Ok maybe_cr -> Ok (Option.value_exn maybe_cr)
      | Error e -> Error e)

  (* returns list of sub term of callees *)
  let get (sub : sub term) (proj : Project.t) sol : rel Or_error.t list =
    get_callees sub proj sol
end
