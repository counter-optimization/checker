open Core
open Bap.Std
open Graphlib.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge
module ABI = Common.AMD64SystemVABI

module TidSet = Set.Make_binable_using_comparator(Tid)

open KB.Monad_infix

type t = Set.M(Tid).t Tid_map.t

let is_def_of_flag defterm =
  let lhs = Var.name @@ Def.lhs defterm in
  ABI.var_name_is_flag lhs

let is_def_not_of_flag defterm =
  let lhs = Var.name @@ Def.lhs defterm in
  not @@ ABI.var_name_is_flag lhs

let is_elt_def_flag = function
  | `Def d -> is_def_of_flag d
  | _ -> false

let has_flags flagmap tid_of_def =
  match Tid_map.find flagmap tid_of_def with
  | Some flagset -> not @@ Set.is_empty flagset
  | None -> false

let get_flags_of_def_tid flagmap tid_of_def =
  match Tid_map.find flagmap tid_of_def with
  | Some flagset -> flagset
  | None -> Set.empty (module Tid)

(* a tid is a key of returned member of type t 
   iff it is not a flag definition *)
let run () =
  let flagmap = ref (Tid_map.empty) in
  let () = Toplevel.exec begin
      KB.objects T.Program.cls >>= fun labels ->
      KB.Seq.iter labels ~f:(fun label ->
          KB.collect T.Semantics.slot label >>= fun sema ->
          let terms = KB.Value.get Term.slot sema in
          KB.return @@ List.iter terms ~f:(fun blkterm ->
                           let (nonflags, flags) = Seq.fold (Blk.elts blkterm) ~init:(TidSet.empty, TidSet.empty)
                             ~f:(fun (nonflags, flags) elt ->
                               match elt with
                               | `Def d ->
                                  let thistid = Term.tid d in
                                  if is_def_of_flag d
                                  then
                                    (nonflags, TidSet.add flags thistid)
                                  else
                                    (TidSet.add nonflags thistid, flags)
                               |  _ -> (nonflags, flags))
                           in
                           TidSet.iter nonflags ~f:(fun nonflagtid ->
                               flagmap := Tid_map.set !flagmap ~key:nonflagtid ~data:flags)))
             end
  in
  !flagmap

