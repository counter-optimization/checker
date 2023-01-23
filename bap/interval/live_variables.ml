open Core
open Bap.Std
open Graphlib.Std
open Common

module ABI = Common.ABI

(* what does it mean for a var in BAP IR SSA to be live?
   we are doing a may-be-live analysis:
   - a non-flag variable is live if it or its flags are used
   - a non-flag variable is not live if it is only used to calculate its flags or if it is not used at all
   - a flag variable is live if it is used
   
   this means that there needs to be a way to determine: given a variable, what is its flags?
   I think for now, we can assume that the flags are set immediately after a var's def.
   so a set of flag defs 'belong' to a var's def if:
   - they are immediately after the var's def
   - the var is used in the flag defs
   
   how can we tell if a def is a flag in SSA?
   in SSA, flags should look like CF.1, CF.2, etc.
   I think for now: get the var name of the lhs, check if it contains CF, PF, AF, ZF, SF, OF

   does BAP IR SSA end up using expressions more than just a single variable's 'def' to
   set the flags?
   answer: yes, see this code snippet (though it is not in SSA):
   iter_insns--Def: 000001e4: RCX := pad:64[low:32[RAX]]
   iter_insns--Def: 000001ee: RAX := pad:64[mem[RBP - 8, el]:u32]
   iter_insns--Def: 00000210: #12582899 := low:32[RAX]
   iter_insns--Def: 00000214: #12582898 := low:32[RCX]
   iter_insns--Def: 00000219: RAX := pad:64[low:32[RAX] + #12582898]
   iter_insns--Def: 0000021e: CF := low:32[RAX] < #12582899
   iter_insns--Def: 00000227: OF := high:1[#12582899] = high:1[#12582898] &
   (high:1[#12582899] | high:1[low:32[RAX]]) &
   ~(high:1[#12582899] & high:1[low:32[RAX]])
   iter_insns--Def: 0000022d: AF := 0x10 = (0x10 & (low:32[RAX] ^ #12582899 ^ #12582898))
   iter_insns--Def: 00000232: PF :=
   ~low:1[let $3 = low:32[RAX] >> 4 ^ low:32[RAX] in
   let $4 = $3 >> 2 ^ $3 in $4 >> 1 ^ $4]
   so the auxilliary carry flag is set using three vars.

   how do we deal with Phi nodes?
   - we can just say a var is live if it is assigned in a phi node. since it is may-be-live, then
   this is never unsound. this is simple
   - we can say a var is live if it or its flags or its phi defs are used. but it may be more complicated.
   
   this is interesting since there is a pattern that could make the more complicated way and the general
   way a bit simpler:
   we run a 'is-used' analysis first.
   this analysis:
   1) looks at each expr on the rhs of a def or phi node
   2) records a four-tuple containg:
   (a, x, y, b)
   where
   a := the var on the rhs being used in this def or phi node's rhs
   x := the TID of the blk elt defining a
   y := the TID of the blk elt at which it is used
   b := the var on the lhs being defined in this def or phi node's lhs

   then after our 'is-used' analysis is ran, a def is live if:
   - there exists an 'is-used' tuple (a, x, y, b) where a is the
   var def and b is not a flag or a phi def
   - in the transitive closure in the relation (from interpreting the 'is-used' tuples
   as a relation), there exists (a, x, y, b) where a is the var def and b is not a flag or
   a phi def
   i think these could be checked in either order, but checking the first condition
   and then the second condition is for sure safe.

   how then do you know if a def is a phi def? i haven't seen any so far because all
   analyses have been done *not* in SSA form.

   the next question is what should the live variable analysis results look like to the
   rest of the analyses?
   1) for the comp simp transforms, we want to be able to query if any of a def's flags are live
   2) for path sensitivity in comp simp checking, we want to be able to query if a flag is live
      (same thing as checking if a var is live)
   3) for optimizing analysis (e.g., if a def is not used, then a sound and efficient
   transfer function for that specific def is the identity function)
   (though this use case is least important and i might not even get around to this since
   i would assume that most non-flag defs are live in the crypto code unless the BAP IR lifter
   is really rough)

   for use case 1):
   it would be nice if there is a wrapper function that says, 'are any of this def's flags live?'
   this would mean we also need an internal function that says 'get this def's flags', then
   this is just an 'any' or 'fold' over the def's flags using the is-used rel trans closure

   for use case 2):
   this is just querying if a variable is live

   for use case 3):
   this is just queryinf if a variable is live

   so we need a public API call to tell if a variable is live.
   big thing tho is that the analyses are *NOT* in SSA, so life would be a lot easier if after running
   the live variable analysis, the results were transformed back out of SSA using the x and y in (a, x, y, b) (aka
   the def or phi elt's tid).

   we should also have a public API call to grab a def's flag defs since
   this uses both liveness analysis and the phi nodes

   the final live variable analysis is then:
   0) convert to ssa
   1) one pass to collect all vars defd in the rhs of a phi node. Phi.var and Phi.options
   this is put in a Set of strings denoted the SSA var names
   2) one pass to collect all 'is-used' tuples, this is put in a Set of is-used tuples
   3) for all the is-used tuples, (a, x, y, b), put x in a Map (varname -> Set is-used-tuple),
   this will be the main data struct in computing the 'is-used' rel's transitive closure
   the varname key is a SSA form string of the variable's name
   the tuple set's 'type' is:
   (SSA var name string * tid of where x was defd * tid of use location * SSA var name string of def using x)
   4) one pass to transform the is-used tuples into tuples of the 'type':
   (var name string * tid of where x was defd * tid of use location * var name of using def)
   where all var names are no longer in SSA form
   4) one pass to collect all def's flag defs: these will be put in a Map (varname-non-ssa -> Set varname-non-ssa)

   note that this is going to be an intra-procedural may-be-live analysis.
   callers are going to be running over an inter-procedural analyses, so
   the inter-procedural analyses will have to be modified to have some concept
   of "S is the current sub i'm in" and "S's liveness analyis is in la-state"
 *)

module T = Bap_core_theory.Theory

(* module NameAndTid = struct *)
(*   module T = struct *)
(*     type t = string * Tid.t [@@deriving compare, sexp, equal, bin_io] *)
(*   end *)

(*   module Cmp = struct *)
(*     include T *)
(*     include Comparator.Make(T) *)
(*   end *)

(*   include T *)
(*   include Cmp *)

(*   let make ~name ~tid : t = (name, tid) *)

(*   module Set = struct *)
(*     include Set.Make_binable_using_comparator(Cmp) *)
(*   end *)
(* end *)

let sub_to_ssa_sub : Sub.t -> Sub.t = Sub.ssa

let fold_over_elts (sub : Sub.t) ~(init : 'a) ~(f:'a -> Blk.elt -> 'a) ~(cong : 'a -> 'a -> 'a) : 'a =
  let blks = Term.enum blk_t sub in
    Seq.fold blks ~init
      ~f:(fun defs blk ->
        let elts = Blk.elts blk in
        let defs' = Seq.fold elts ~init:defs ~f in
        cong defs defs')

let get_all_defs (sub : sub term) : def term list =
  let blks = Term.enum blk_t sub |> Seq.to_list in
  List.map blks ~f:(fun b -> Term.enum def_t b |> Seq.to_list)
  |> List.join

let get_all_phis (sub : sub term) : phi term list =
  let blks = Term.enum blk_t sub |> Seq.to_list in
  List.map blks ~f:(fun b -> Term.enum phi_t b |> Seq.to_list)
  |> List.join

(* internal passes used for intermediate data gathering *)
(* module GetPhiAssnsPass = struct *)
(*   let elt_to_phi_var : Blk.elt -> NameAndTid.Set.t = function *)
(*     | `Phi p -> *)
(*        let name = (Phi.var p |> T.Var.name) in *)
(*        let tid = Term.tid p in  *)
(*        NameAndTid.make ~name ~tid |> NameAndTid.Set.singleton *)
(*     | _ -> NameAndTid.Set.empty *)

(*   (\* returns set of tuples of phi defs *)
(*    * and the tid of the phi: this is defd here *\) *)
(*   let run (sub : Sub.t) : NameAndTid.Set.t = *)
(*     fold_over_elts sub *)
(*       ~init:NameAndTid.Set.empty *)
(*       ~cong:NameAndTid.Set.union *)
(*       ~f:(fun defs elt -> *)
(*         elt_to_phi_var elt |> NameAndTid.Set.union defs) *)
(* end *)

(* create a map from var name to the tid where it is defined. this
   is the GetDefsPass.t type *)
module GetDefsPass : sig
  type t

  val def_tid_of_var_name : t -> string -> Tid.t option
  val run : Sub.t -> t
end = struct
  module M = Map.Make_plain_using_comparator(String)

  type t = Tid.t M.t

  let combine ~key which =
    match which with
    | `Both (left, right) ->
       if Tid.equal left right
       then Some left
       else failwith "tids shouldn't be duplicated in GetDefsPass"
    | `Left l -> Some l
    | `Right r -> Some r

  let add_def_to_mapping mapping elt =
    match elt with
    | `Def d ->
       let lhs = Def.lhs d in
       let name = Var.name lhs in
       let this_tid = Term.tid d in
       M.set mapping ~key:name ~data:this_tid
    | `Phi p ->
       let lhs = Phi.lhs p in
       let name = Var.name lhs in
       let this_tid = Term.tid p in
       M.set mapping ~key:name ~data:this_tid
    | _ -> mapping

  let def_tid_of_var_name (m : t) (name : string) : Tid.t option =
    M.find m name
    (* match M.find m name with *)
    (* | None -> *)
    (*    let err_s = sprintf "Couldn't find tid of def/phi for var %s in  GetDefsPass.def_tid_of_var_name" name *)
    (*    in *)
    (*    failwith err_s *)
    (* | Some t -> t *)
  
  let run (sub : Sub.t) : t =
    fold_over_elts sub
      ~init:M.empty
      ~cong:(M.merge ~f:combine)
      ~f:add_def_to_mapping
end

(*
  (a, x, y, b)
   where
   a := the var on the rhs being used in this def or phi node's rhs
   x := the TID of the blk elt defining a
   y := the TID of the blk elt at which it is used
   b := the var on the lhs being defined in this def or phi node's lhs
  *)
module IsUsedPass = struct
  module T = struct
    (* used_tid is an option type since subs in SSA still have some free
       vars (the sub's arguments) *)
    type t = { used : string;
               used_defining_tid : tid option;
               user_tid : tid;
               user : string }
               [@@deriving sexp, compare, bin_io]
  end

  module Cmp = struct
    include T
    include Comparator.Make(T)
  end
  
  module UseRel = struct
    include Set.Make_using_comparator(Cmp)
  end
  
  include Cmp
             
  let phi_option_to_uses (_tid, opt_expr) : SS.t = Var_name_collector.run opt_expr

  let def_to_uses (d : def term) (defmap : GetDefsPass.t) : t list =
    let rhs = Def.rhs d in
    let user = Def.lhs d |> Var.name in
    let used_names = Var_name_collector.run rhs in
    let user_tid = Term.tid d in
    Set.to_list used_names
    |> List.map ~f:(fun used ->
                  let used_defining_tid = GetDefsPass.def_tid_of_var_name defmap used in
                  { used; user_tid; used_defining_tid; user })

  let phi_to_uses (p : phi term) (defmap : GetDefsPass.t) : t list =
    let options = Phi.values p in
    let used_names = Seq.fold options ~init:SS.empty
                              ~f:(fun uses opt ->
                                phi_option_to_uses opt |> SS.union uses) in
    let user_tid = Term.tid p in
    let user = Phi.lhs p |> Var.name in
    Set.to_list used_names
    |> List.map ~f:(fun used ->
                  let used_defining_tid = GetDefsPass.def_tid_of_var_name defmap used in
                  { used; user_tid; used_defining_tid; user })

  let to_string (rel : t) : string =
    let user_tid_s = Tid.to_string rel.user_tid in
    let used_tid_s = match rel.used_defining_tid with
      | None -> "none,freevar"
      | Some tid -> Tid.to_string tid in
    sprintf "(used: %s, used_defining_tid: %s, user_tid: %s, user: %s)"
      rel.used used_tid_s user_tid_s rel.user

  let print_rel (rel : t) : unit = printf "%s\n%!" @@ to_string rel
  
  let print_rels (rels : UseRel.t) : unit =
    printf "Used-By relations are:\n%!";
    UseRel.iter rels ~f:print_rel

  (* returns a set of tuples of vars used and the
     tid of where it is used *)
  (* defs mapping is map from varname -> tid of where it is defd *)
  let run (sub : Sub.t) (defs_mapping : GetDefsPass.t) : UseRel.t =
    let all_defs = get_all_defs sub in
    let all_phis = get_all_phis sub in
    let all_def_usages = List.map all_defs ~f:(fun d -> def_to_uses d defs_mapping)
                         |> List.join in
    let all_phi_usages = List.map all_phis ~f:(fun p -> phi_to_uses p defs_mapping)
                         |> List.join in
    let rel_list = List.append all_phi_usages all_def_usages in
    UseRel.of_list rel_list
end

type t = IsUsedPass.UseRel.t

(* the live variables analysis (Live_variables.Analysis.run) *)
module Analysis = struct
  let run (sub : Sub.t) : IsUsedPass.UseRel.t =
    let sub_ssa = sub_to_ssa_sub sub in
    let defs_map = GetDefsPass.run sub_ssa in
    let used_by_rels = IsUsedPass.run sub_ssa defs_map in
    used_by_rels
end

type name_and_tid = string * tid

let unssa_var_name (name : string) : string =
  let ssa_suffix_start_char = '.' in
  match String.rindex name ssa_suffix_start_char with
  | Some idx_of_ssa_start -> String.slice name 0 idx_of_ssa_start
  | None -> name

let filter_results (analysis_results : IsUsedPass.UseRel.t) ~(f : IsUsedPass.t -> bool) : t =
  Set.filter analysis_results ~f

let get_users_names_and_tids (analysis_results : IsUsedPass.UseRel.t) ~(of_tid : tid) : name_and_tid list =
  (* let def_tid = Term.tid of_def in *)
  let is_use_rel_for_def_tid (use_rel : IsUsedPass.t) : bool =
    match use_rel.used_defining_tid with
    | None -> false
    | Some other_def_tid -> Tid.equal other_def_tid of_tid in
  filter_results analysis_results ~f:is_use_rel_for_def_tid
  |> Set.to_list
  |> List.map ~f:(fun (use_rel : IsUsedPass.t) ->
                (unssa_var_name use_rel.user, use_rel.user_tid))

(* is this def used in some other non-flag-defining def? *)
let is_live_flagless (analysis_results : IsUsedPass.UseRel.t) ~(tid : tid) : bool =
  let users : (string * tid) list =
    get_users_names_and_tids analysis_results ~of_tid:tid in
  let is_not_flag (var_name : string) : bool =
    let normalized_name = unssa_var_name var_name in
    not @@ ABI.var_name_is_flag normalized_name in
  let non_flag_users : (string * tid) list =
    List.filter users ~f:(fun (name, tid) -> is_not_flag name) in
  not @@ List.is_empty non_flag_users
  (* if not @@ List.is_empty non_flag_users *)
  (* then true *)
  (* else failwith "todo" *)

(* bool option: if the variable has no flags *)
(* assumption: flags for one instruction are not used
   to calculate the flags for another instruction *)
let get_live_flags_of_prev_def_tid (analysis_results : IsUsedPass.UseRel.t) ~(prev_def_tid : tid) : SS.t =
  let all_users : (string * tid) list =
    get_users_names_and_tids analysis_results ~of_tid:prev_def_tid in
  let is_flag (var_name : string) : bool = 
    let normalized_name = unssa_var_name var_name in
    ABI.var_name_is_flag normalized_name in
  let flag_users : (string * tid) list =
    List.filter all_users ~f:(fun (name, tid) -> is_flag name) in
  let live_flag_users : (string * tid) list =
    List.filter flag_users ~f:(fun (name, tid) ->
                  is_live_flagless analysis_results ~tid) in
  let live_flag_users_normalized_names : string list =
    List.map live_flag_users ~f:(fun (name, _) -> unssa_var_name name) in
  SS.of_list live_flag_users_normalized_names

(* let get_dependent_defs_up_to_bound  *)
  
