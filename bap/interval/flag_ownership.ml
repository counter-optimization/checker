open Core_kernel
open Bap.Std
open Graphlib.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge
module ABI = Common.AMD64SystemVABI

module TT = struct
  type t = Tid.Set.t Tid_map.t [@@deriving sexp, compare, equal]
end

include TT

let equal = TT.equal

let sexp_of_t = TT.sexp_of_t

let empty = Tid_map.empty

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
  | Some flagset -> not @@ Tid.Set.is_empty flagset
  | None -> false

let get_flags_of_def_tid flagmap tid_of_def =
  match Tid_map.find flagmap tid_of_def with
  | Some flagset -> flagset
  | None -> Tid.Set.empty

let defs_of_flags (env : t) : Tid.Set.t Tid.Map.t =
  Tid.Map.fold env
    ~init:Tid.Map.empty
    ~f:(fun ~key:def ~data:flags revmap ->
      Tid.Set.fold flags ~init:revmap
        ~f:(fun revmap flagtid ->
          Tid.Map.update revmap flagtid ~f:(function
            | Some prevdefs -> Tid.Set.add prevdefs def
            | None -> Tid.Set.singleton def)))

let get_flags (type a) (t : a Term.t) =
  let init = (Tid.Set.empty, Tid.Set.empty) in
  let flags_of_blk blk =
    Seq.fold (Term.enum def_t blk) ~init ~f:(fun (n,f) dt ->
      let tid = Term.tid dt in
      if is_def_of_flag dt
      then (n, Tid.Set.add f tid)
      else (Tid.Set.add n tid, f))
  in
  match Term.get_attr t Disasm.insn with
  | Some sema ->
    let ir = KB.Value.get Term.slot sema in
    List.fold ir ~init ~f:(fun st blk -> flags_of_blk blk)
  | None -> init

module Pass : sig
  include Uc_single_shot_pass.PASS with type t = TT.t
end = struct
  type t = TT.t

  type _ Uc_single_shot_pass.key += Key : t Uc_single_shot_pass.key
             
  let default () = Tid.Map.empty
                     
  let ondef dt st =
    let tid = Term.tid dt in
    if Tid.Map.mem st tid
    then st
    else
      let (nf, f) = get_flags dt in
      Tid.Set.fold nf ~init:st ~f:(fun st nftid ->
        Tid.Map.set st ~key:nftid ~data:f)

  let onjmp dt st = st
  let onphi dt st = st
end

let () = Uc_single_shot_pass.GroupedAnalyses.register_runner (module Pass)
