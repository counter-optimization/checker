open Core_kernel
open Bap.Std

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
   x := the TID of the blk elt defining x
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

module NameAndTid = struct
  module T = struct
    type t = string * Tid.t [@@deriving compare, sexp, equal, bin_io]
  end

  module Cmp = struct
    include T
    include Comparator.Make(T)
  end

  include T
  include Cmp

  let make ~name ~tid : t = (name, tid)

  module Set = struct
    include Set.Make_binable_using_comparator(Cmp)
  end
end

module SS = Set.Make_binable_using_comparator(String)

let sub_to_ssa_sub : Sub.t -> Sub.t = Sub.ssa

let fold_over_elts (sub : Sub.t) ~(init : 'a) ~(f:'a -> Blk.elt -> 'a) : 'a =
  let blks = Term.enum blk_t sub in
    Seq.fold blks ~init:NameAndTid.Set.empty
      ~f:(fun defs blk ->
        let elts = Blk.elts blk in
        let defs' = Seq.fold elts ~init:defs ~f in
        NameAndTid.Set.union defs defs')

(* internal passes used for intermediate data gathering *)
module GetPhiAssnsPass = struct
  let elt_to_phi_var : Blk.elt -> NameAndTid.Set.t = function
    | `Phi p ->
       let name = (Phi.var p |> T.Var.name) in
       let tid = Term.tid p in 
       NameAndTid.make ~name ~tid |> NameAndTid.Set.singleton
    | _ -> NameAndTid.Set.empty
  
  let run (sub : Sub.t) : NameAndTid.Set.t =
    fold_over_elts sub ~init:NameAndTid.Set.empty
      ~f:(fun defs elt ->
        elt_to_phi_var elt |> NameAndTid.Set.union defs)
end

module GetDefsPass = struct
  let run (sub : Sub.t) : NameAndTid.Set.t =
    NameAndTid.Set.empty
end

module IsUsedPass = struct
  let phi_option_to_uses (_tid, opt_expr) : SS.t =
    Var_name_collector.run opt_expr

  let associate_uses_with_tid (uses : SS.t) (tid : Tid.t)
      : NameAndTid.Set.t =
    SS.fold uses ~init:NameAndTid.Set.empty
         ~f:(fun acc name ->
           let name_with_tid = name, tid in
           NameAndTid.Set.add acc name_with_tid)
  
  let elt_to_uses : Blk.elt -> NameAndTid.Set.t = function
    | `Def d ->
       let rhs = Def.rhs d in
       let used_names = Var_name_collector.run rhs in
       let defs_tid = Term.tid d in
       associate_uses_with_tid used_names defs_tid
    | `Phi p ->
       let options = Phi.values p in
       let used_names = Seq.fold options ~init:SS.empty
                          ~f:(fun uses opt ->
                            phi_option_to_uses opt |> SS.union uses)
       in
       let phis_tid = Term.tid p in
       associate_uses_with_tid used_names phis_tid
    | _ -> NameAndTid.Set.empty

  let run sub =
    fold_over_elts sub ~init:NameAndTid.Set.empty
      ~f:(fun uses elt ->
        elt_to_uses elt |> NameAndTid.Set.union uses)
end

(* the live variables analysis (Live_variables.Analysis.run) *)
module Analysis = struct
  let run (sub : Sub.t) : unit =
    let sub_ssa = sub_to_ssa_sub sub in
    printf "non-ssa sub insns are: \n";
    Edge_builder.iter_insns sub;
    printf "*****************************\n";
    printf "ssa sub insns are: \n";
    Edge_builder.iter_insns sub_ssa
end
