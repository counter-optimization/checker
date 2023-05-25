open Core
open Bap.Std
open Graphlib.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge
module ABI = Common.AMD64SystemVABI

open KB.Monad_infix

let is_def_of_flag defterm =
  let lhs = Var.name @@ Def.lhs defterm in
  ABI.var_name_is_flag lhs

let is_def_not_of_flag defterm =
  let lhs = Var.name @@ Def.lhs defterm in
  not @@ ABI.var_name_is_flag lhs

(* let get_flag_own_tids defterm = *)
(*   if is_def_of_flag defterm *)
(*   then *)
(*     [] *)
(*   else *)
(*     let tid = Term.tid defterm in *)
(*     let all_flag_tids = ref [] in *)
(*     let () = Toplevel.exec begin *)
(*       KB.collect T.Semantics.slot tid >>= fun insn -> *)
(*       let terms = KB.Value.get Term.slot insn in *)
(*       KB.return @@ List.iter terms ~f:(fun blk -> *)
(*       Seq.iter (Blk.elts blk) ~f:(function *)
(*         | `Def innerterm -> *)
(*            printf "tid has term: %a\n%!" Def.ppo innerterm; *)
(*            if is_def_of_flag innerterm *)
(*            then *)
(*              let flagdeftid = Term.tid innerterm in *)
(*              printf "flag tid: %a\n%!" Tid.ppo flagdeftid; *)
(*              all_flag_tids := flagdeftid :: !all_flag_tids *)
(*            else *)
(*              () *)
(*         | _ -> ())) *)
(*                end *)
(*     in *)
(*     !all_flag_tids *)

module TidSet = Set.Make_binable_using_comparator(Tid)

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

