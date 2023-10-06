open Core_kernel
open Bap.Std
open Graphlib.Std

module KB = Bap_knowledge.Knowledge
module T = Bap_core_theory.Theory

open KB.Monad_infix

type t

let package = "uarch-checker"

let log_prefix = sprintf "%s.dmp_helpers" package
module L = struct
  include Dolog.Log
  let () = set_prefix log_prefix
end

let checker_blacklisted_fns = ["smalloc";
                               "sodium_smalloc";
                               "sfree";
                               "updateStackAddr";
                               "unsetBit";
                               "smemcpy";
                               "handle64BitStore"]

type subname = string
let is_blacklisted (name : subname) : bool =
  let prefix = "__mrd_global_ctor" in
  String.Caseless.is_prefix ~prefix name

let smalloc_addr_clz : (t, unit) KB.cls = KB.Class.declare
                                            ~public:true
                                            ~package
                                            "potential-smalloc-addrs"
                                            ()

let smalloc_addr_slot = KB.Class.property
                          ~public:true
                          ~package
                          smalloc_addr_clz
                          "addr"
                          (KB.Domain.powerset (module Word) "addr-powset")

let smalloc_addr_sym = "smalloc-addr"

let get_smalloc_addrs () =
  KB.Symbol.intern ~package smalloc_addr_sym smalloc_addr_clz >>= fun obj ->
  KB.collect smalloc_addr_slot obj

let print_smalloc_addrs () =
  Toplevel.exec begin
    get_smalloc_addrs () >>= fun addrs ->
    L.debug "%s" "smalloc addrs are:";
    Word.Set.iter addrs ~f:(L.debug "\t%a" Word.ppo);
    KB.return ()
  end

let find_smalloc proj =
  let open Image.Scheme in
  let filename = Option.value_exn (Project.get proj filename) in
  let try_find_all_smallocs ogredoc =
    let external_syms = Ogre.Query.(select (from external_reference)
                                      ~where:(external_reference.(name) = str "smalloc" ||
                                              external_reference.(name) = str "sodium_smalloc" ||
                                              external_reference.(name) = str "updateStackAddr")) in
    let internal_syms = Ogre.Query.(select (from named_symbol)
                                      ~where:(named_symbol.(name) = str "smalloc" ||
                                              named_symbol.(name) = str "sodium_smalloc" ||
                                              named_symbol.(name) = str "updateStackAddr")) in
    Or_error.(Ogre.eval (Ogre.collect external_syms) ogredoc >>= fun ext ->
              Ogre.eval (Ogre.collect internal_syms) ogredoc >>= fun int ->
              Ok (Seq.append ext int)) in
  let addrs_already_resolved () =
    get_smalloc_addrs () >>= fun addrs ->
    KB.return @@ (addrs, not @@ Set.is_empty addrs) in
  Toplevel.exec begin
    addrs_already_resolved () >>= fun (addrs, addrs_already_resolved) ->
    if addrs_already_resolved
    then KB.return ()
    else
      T.Unit.for_file filename >>= fun unit ->
      KB.collect Image.Spec.slot unit >>= fun ogre ->
      match try_find_all_smallocs ogre with
      | Ok qr when Seq.is_empty qr ->
        failwith "[Dmp_helpers] Couldn't find smalloc virtual address : %s"
      | Ok qr -> 
        let addrs = Seq.fold qr ~init:addrs ~f:(fun addrs (a, n) ->
          Word.Set.add addrs @@ Word.of_int64 ~width:64 a) in
        KB.Symbol.intern ~package smalloc_addr_sym smalloc_addr_clz >>= fun obj ->
        KB.provide smalloc_addr_slot obj addrs
      | Error err ->
        let e = sprintf "[Dmp_helpers] ogre query error for smalloc : %s"
                  (Error.to_string_hum err) in
        failwith e
  end

let is_smalloc_call (addr : int) : bool =
  let res = ref false in
  Toplevel.exec begin
    get_smalloc_addrs () >>= fun addrs ->
    let addr = Word.of_int ~width:64 addr in
    KB.return (res := Word.Set.mem addrs addr)
  end;
  !res

type guard_point = {
  var : string;
  set : bool;
  location : tid;
}

type guard = {
  var : string;
  set : bool
}

type gmentry = guard list ref

type guard_map = gmentry Tid.Map.t

let guard_to_string {var;set;location} =
  let set = if set then "1" else "0" in
  let tid = Tid.to_string location in
  Format.sprintf "<%s, %s, %s>" var set tid

let print_guards guards =
  let rec loop = function
    | g :: gs ->
      L.debug "\t%s" (guard_to_string g);
      loop gs
    | [] -> ()
  in
  L.debug "%s" "guards:";
  loop guards

let needs_set : guard_map -> tid -> bool = Tid.Map.mem
let get_guard : guard_map -> tid -> gmentry option = Tid.Map.find

(* 6th insn after `CF := low:1(VAR >> 0x3C)` is 
   the condition jmp
   7th insn after is the fall through *)
let find_guard_points sub =
  let rshift_bits width = Word.of_int ~width 0x3c in
  let is_cf_assn d =
    String.Caseless.equal "cf" @@ Var.name @@ Def.lhs d
  in
  let is_bt_3c = function
    | Bil.Cast (Bil.LOW, 1, (Bil.BinOp (Bil.RSHIFT, Bil.Var v, Bil.Int w))) ->
      let width = Word.bitwidth w in
      if Word.equal (rshift_bits width) w
      then Some (Var.name v)
      else None
    | _ -> None
  in
  let elt_to_tid = function
  | `Jmp j -> Term.tid j
  | `Def d -> Term.tid d
  | `Phi p -> Term.tid p
  in
  let get_jmp_targets (insns : Blk.elt Seq.t) var =
    let fail s =
      let tid = (Seq.hd_exn insns |> elt_to_tid) in
      L.error "couldn't get jmp targets for dmp pointer bit test %s at %a"
                    s Tid.ppo tid;
      []
    in
    let bit_cleared = Seq.drop insns 6 in
    let bit_set = Seq.drop bit_cleared 1 in
    match Seq.hd bit_cleared, Seq.hd bit_set with
    | Some (`Jmp cl), Some (`Jmp set) ->
      begin match Jmp.kind cl, Jmp.kind set with
      | Goto (Direct cltid), Goto (Direct settid) ->
        [{ var; set=false; location=cltid };
         { var; set=true; location=settid }]
      | _ -> fail "1"
      end
    | _, _ -> fail "2"
  in
  let rec get_guards ?(guards = []) insns =
    let open Option.Monad_infix in
    match insns with
    | None -> guards
    | Some insns ->
      let rst = Seq.tl insns in
      match Seq.hd insns with
      | Some (`Def d) when is_cf_assn d ->
        let exp = Def.rhs d in
        begin match is_bt_3c exp with
        | Some var ->
          let new_targets = get_jmp_targets insns var in
          get_guards rst ~guards:(new_targets @ guards)
        | None -> get_guards rst ~guards
        end
      | Some (`Def d) -> get_guards rst ~guards
      | _ -> guards
  in
  let guards_to_map (gs : guard_point list) : guard_map =
    List.fold gs ~init:Tid.Map.empty
      ~f:(fun m {var;set;location} ->
        match Tid.Map.find m location with
        | Some guards ->
          guards := {var;set} :: !guards;
          m
        | None ->
          let guards = ref [{var;set}] in
          Tid.Map.set m ~key:location ~data:guards)
  in
  let irg = Sub.to_cfg sub in
  let rpo = Graphlib.reverse_postorder_traverse (module Graphs.Ir) irg in
  let guards = Seq.fold rpo ~init:[] ~f:(fun guards node ->
    let blk = Graphs.Ir.Node.label node in
    let elts = Blk.elts blk in
    let new_guards = get_guards (Some elts) in
    new_guards @ guards)
  in
  print_guards guards;
  guards_to_map guards
      
  
