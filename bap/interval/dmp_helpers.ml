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

let mk_guard ~var ~set : guard = { var;set }

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

let elt_to_tid = function
  | `Jmp j -> Term.tid j
  | `Def d -> Term.tid d
  | `Phi p -> Term.tid p

module FindSafePtrBitTestPass : sig
  
  include Uc_single_shot_pass.PASS
            
  val get_guard_points : 'a. t -> 'a -> ('a -> tid -> Tid.Set.t) -> guard_map
    
end = struct
  type varname = string
    
  type jmp_target = tid
    
  type c_or_s = Clear of jmp_target | Set of jmp_target
                  
  type t = {
    bt_tids : varname Tid.Map.t;
    jmps : c_or_s Tid.Map.t;
    fallthrough : tid Tid.Map.t;
    clear_jmp : tid option;
  }

  type _ Uc_single_shot_pass.key += Key : t Uc_single_shot_pass.key

  let get_fallthrough st clearjmptid : c_or_s option =
    let open Option.Monad_infix in
    Tid.Map.find st.fallthrough clearjmptid >>= fun fallthroughtid ->
    Tid.Map.find st.jmps fallthroughtid

  type exn += GuardPointNoJmpTid | NoReachingJmpTid
  let get_guard_points (type a) (st : t) (rds : a) (getrts : a -> tid -> Tid.Set.t) : guard_map =
    let init : guard_map = Tid.Map.empty in
    Tid.Map.fold st.bt_tids ~init ~f:(fun ~key ~data acc ->
      let var = data in
      let rts = getrts rds key in
      let jmptids = Tid.Map.key_set st.jmps in
      let thisjmptids = Tid.Set.inter rts jmptids in
      (if Tid.Set.is_empty thisjmptids
       then L.error "bittest at %a has no rts jumptids"
              Tid.ppo key
       else Tid.Set.iter thisjmptids
              ~f:(L.info "bittest %a has jmptid %a"
                    Tid.ppo key Tid.ppo));
      (if Tid.Set.is_empty thisjmptids
       then raise NoReachingJmpTid);
      Tid.Set.fold thisjmptids ~init:acc
        ~f:(fun guards jmpt ->
          let (guard, target) = match Tid.Map.find st.jmps jmpt with
            | Some (Clear target) -> (mk_guard ~var ~set:false, target)
            | Some (Set target) -> (mk_guard ~var ~set:true, target)
            | None -> raise GuardPointNoJmpTid
          in
          let (ftguard, fttarget) = match get_fallthrough st jmpt with
            | Some (Clear target) -> (mk_guard ~var ~set:false, target)
            | Some (Set target) -> (mk_guard ~var ~set:true, target)
            | None -> raise GuardPointNoJmpTid
          in
          let guards = match Tid.Map.find guards target with
            | Some otherguards ->
              otherguards := guard :: !otherguards;
              guards
            | None ->
              let gs = ref [guard] in
              Tid.Map.set guards ~key:target ~data:gs
          in
          match Tid.Map.find guards fttarget with
          | Some otherguards ->
            otherguards := ftguard :: !otherguards;
            guards
          | None ->
            let gs = ref [ftguard] in
            Tid.Map.set guards ~key:fttarget ~data:gs))

  let default () : t = {
    bt_tids = Tid.Map.empty;
    jmps = Tid.Map.empty;
    fallthrough = Tid.Map.empty;
    clear_jmp = None;
  }

  let onphi pt st = st
    
  let ondef dt st =
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
    let st = { st with clear_jmp = None } in
    let rhs = Def.rhs dt in
    if is_cf_assn dt
    then match is_bt_3c rhs with
      | Some varname ->
        let tid = Term.tid dt in
        { st with
          bt_tids = Tid.Map.set st.bt_tids
                      ~key:tid
                      ~data:varname }
      | None -> st
    else st

  type exn += SetBrNoTarget | ClrBrNoTarget
  let onjmp jt st =
    let is_cf v = String.Caseless.equal "cf" @@ Var.name v in
    let is_cf_check = function
      | Bil.UnOp (_, (Bil.Var v)) when is_cf v -> true
      | _ -> false
    in
    let get_jmp_target jt =
      match Jmp.kind jt with
      | Goto (Direct tid) -> Some tid
      | _ -> None
    in
    if is_cf_check @@ Jmp.cond jt 
    then
      let key = Term.tid jt in
      match get_jmp_target jt with
      | Some tid ->
        let data = Clear tid in
        { st with
          clear_jmp = Some key;
          jmps = Tid.Map.set st.jmps ~key ~data }
      | None ->
        L.error "Clr jmp branch with no target";
        raise ClrBrNoTarget
    else match st.clear_jmp with
      | Some clearjmptid ->
        let key = Term.tid jt in
        begin match get_jmp_target jt with
        | Some tid ->
          let data = Set tid in
          { st with
            clear_jmp = None;
            fallthrough = Tid.Map.set st.fallthrough
                            ~key:clearjmptid ~data:key;
            jmps = Tid.Map.set st.jmps ~key ~data }
        | None -> L.error "Set jmp branch with no target";
          raise SetBrNoTarget
        end
      | None -> st
end
