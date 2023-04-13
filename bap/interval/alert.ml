open Core
open Bap.Std
open Common
(* open Interval_tree *)

module Theory = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

type reason = CompSimp | SilentStores
                           [@@deriving sexp, bin_io, compare, equal]

let string_of_reason = function
  | CompSimp -> "comp-simp"
  | SilentStores -> "silent-stores"

module T = struct
  (* flags_live: not computed or computed and true or false *)
  (* problematic_operands: doesn't apply or a list of operand indices *)
  type t = {
      sub_name : string option;
      opcode : string option;
      addr : word option;
      rpo_idx : int option;
      tid : Tid.t;
      flags_live : SS.t;
      is_live : bool option;
      problematic_operands : int list option;
      left_val : string option;
      right_val : string option;
      reason : reason;
      desc : string;
      flags_live_in : SS.t
    } [@@deriving sexp, bin_io, compare]
end

module Cmp = struct
  include T
  include Comparator.Make(T)
end

module Set = struct
  include Set.Make_binable_using_comparator(Cmp)
end

include T

let str_of_flags_live (flags_live : SS.t) : string =
  let flags_live = SS.to_list flags_live in
  let rec loop flag_names acc =
    match flag_names with
    | [] -> "" (* e.g., in case flags_live is empty, otherwise never reached *)
    | final_flag_name :: [] -> acc ^ final_flag_name
    | cur_flag_name :: next_flag_name :: rest_flags ->
       let cur_name_with_comma = cur_flag_name ^ "," in
       loop (next_flag_name :: rest_flags) (acc ^ cur_name_with_comma) in
  loop flags_live ""

module type Pass = sig
  val set_for_alert_set : Set.t -> Project.t -> Set.t
end

module OpcodeAndAddrFiller : Pass = struct
  open KB.Monad_infix

  type opcode_alist = (tid * String.t) list [@@deriving compare, sexp, bin_io]

  let opcode_alist_cls : (opcode_alist, unit) KB.cls = KB.Class.declare
                                                   ~public:true
                                                   ~package:Common.package
                                                   "tid_to_opcode_alist"
                                                   ()

  let opcode_alist_domain = KB.Domain.flat
                           ~inspect:sexp_of_opcode_alist
                           ~join:(fun left right -> Ok (List.append right left))
                           ~empty:[]
                           ~equal:(fun left right -> 0 = compare_opcode_alist left right)
                           "opcode_alist_domain"

  let final_opcode_alist : (opcode_alist, opcode_alist) KB.slot = KB.Class.property
                                                           ~public:true
                                                           ~package:Common.package
                                                           opcode_alist_cls
                                                           "final_opcode_alist"
                                                           opcode_alist_domain
  
  type addr_alist = (tid * word) list [@@deriving compare, sexp, bin_io]

  let addr_alist_cls : (addr_alist, unit) KB.cls = KB.Class.declare
                                                     ~public:true
                                                     ~package:Common.package
                                                     "label_to_addr_alist"
                                                     ()
  
  let addr_alist_domain = KB.Domain.flat
                            ~inspect:sexp_of_addr_alist
                           ~join:(fun left right -> Ok (List.append right left))
                           ~empty:[]
                           ~equal:(fun left right -> 0 = compare_addr_alist left right)
                           "addr_alist_domain"

  let final_addr_alist : (addr_alist, addr_alist) KB.slot = KB.Class.property
                                                           ~public:true
                                                           ~package:Common.package
                                                           addr_alist_cls
                                                           "final_addr_alist"
                                                           addr_alist_domain

  type tidmap = (Tid.t, string, Tid.comparator_witness) Map.t
  type addrmap = (Tid.t, word, Tid.comparator_witness) Map.t
  type lut = addrmap * tidmap

  let build_lut (proj : Project.t) : lut =
    let target = Project.target proj in
    let computation =
      KB.objects Theory.Program.cls >>= fun labels ->
      let init_alists : addr_alist * opcode_alist = ([], []) in
      let alists = Seq.fold labels ~init:(KB.return init_alists) ~f:(fun alists label ->
                 alists >>= fun alists ->
                 KB.collect Theory.Semantics.slot label >>= fun sema ->
                 KB.collect Theory.Label.addr label >>= fun maybe_addr ->
                 KB.collect Theory.Semantics.slot label >>= fun (insn : Insn.t) ->
                 let opcode_str = Insn.name insn in
                 let maybe_addr_bitvector =
                   Option.bind maybe_addr ~f:(fun bitv ->
                       Option.return @@ Bitvector.code_addr target bitv)
                 in
                 let addr_str = Option.value maybe_addr_bitvector ~default:(Word.one 64) in
                 let bir_terms = KB.Value.get Term.slot sema in
                 let tids_lists = List.map bir_terms ~f:(fun b ->
                                             Blk.elts b
                                             |> Seq.map ~f:Common.elt_to_tid
                                             |> Seq.to_list) in
                 let tids = List.join tids_lists in
                 let tid_to_addr_alist = List.map tids ~f:(fun tid -> (tid, addr_str)) in
                 let tid_to_opcode_alist = List.map tids ~f:(fun tid -> (tid, opcode_str)) in
                 KB.return (List.append tid_to_addr_alist (fst alists),
                            List.append tid_to_opcode_alist (snd alists))) in

      alists >>= fun (addr_alist, opcode_alist) ->
      KB.Object.create addr_alist_cls >>= fun addr_alist_obj ->
      KB.provide final_addr_alist addr_alist_obj addr_alist >>= fun () ->
      KB.Object.create opcode_alist_cls >>= fun opcode_alist_obj ->
      KB.provide final_opcode_alist opcode_alist_obj opcode_alist >>= fun () ->
      KB.return (addr_alist_obj, opcode_alist_obj) in
    let opcode_alist_obj = computation >>= fun (addrs,ops) -> KB.return ops in
    let addr_alist_obj = computation >>= fun (addrs,ops) -> KB.return addrs in
    let cur_st = Toplevel.current () in
    let (opcode_alist, st') = match KB.run opcode_alist_cls opcode_alist_obj cur_st with
      | Ok (opcode_alist, st') -> opcode_alist, st'
      | Error e ->
         failwith @@ sprintf "In OpcodeAndAddrFiller.build_lut opcodes : %s" @@
           KB.Conflict.to_string e in
    let (addr_alist, st') = match KB.run addr_alist_cls addr_alist_obj cur_st with
      | Ok (addr_alist, st') -> addr_alist, st'
      | Error e ->
         failwith @@ sprintf "In OpcodeAndAddrFiller.build_lut addrs : %s" @@
           KB.Conflict.to_string e in
    let opcode_alist = KB.Value.get final_opcode_alist opcode_alist in
    let addr_alist = KB.Value.get final_addr_alist addr_alist in
    let opcode_lut = match Map.of_alist (module Tid) opcode_alist with
      | `Duplicate_key k ->
         failwith @@ sprintf "In OpcodeAndAddrFiller.build_lut, opcode_lut build, duplicate key: %a" Tid.pps k
      | `Ok lut -> lut in
    let addr_lut = match Map.of_alist (module Tid) addr_alist with
      | `Duplicate_key k ->
         failwith @@ sprintf "In OpcodeAndAddrFiller.build_lut, addr_lut build, duplicate key: %a" Tid.pps k
      | `Ok lut -> lut in
    (addr_lut, opcode_lut)

  let set_addr_for_alert (alert : T.t) (addr_lut : addrmap) : T.t =
    let alert_tid = alert.tid in
    match Map.find addr_lut alert_tid with
    | Some addr_str -> 
       { alert with addr = Some addr_str }
    | None ->
       failwith @@ sprintf "In OpcodeAndAddrFiller.build_lut, set_addr_for_alert, couldn't get addr for alert on tid: %a" Tid.pps alert_tid

  let set_opcode_for_alert (alert : T.t) (opcode_lut : tidmap) : T.t =
    let alert_tid = alert.tid in
    match Map.find opcode_lut alert_tid with
    | Some opcode_str -> 
       { alert with opcode = Some opcode_str }
    | None ->
       failwith @@ sprintf "In OpcodeAndAddrFiller.build_lut, set_opcode_for_alert, couldn't get opcode for alert on tid: %a" Tid.pps alert_tid
                                            
  let set_for_alert_set (alerts : Set.t) (proj : Project.t) : Set.t =
    let lut : lut = build_lut proj in
    let (addr_lut, opcode_lut) = lut in
    Set.map alerts ~f:(fun alert -> set_addr_for_alert alert addr_lut)
    |> Set.map ~f:(fun alert -> set_opcode_for_alert alert opcode_lut)
end

module SubNameResolverFiller = struct

  type cls

  type resolved_db = (word * string) seq [@@deriving compare, sexp]

  let named_symbols : (cls, unit) KB.cls = KB.Class.declare "named-symbols" ()
                                             ~package:Common.package

  let dom = KB.Domain.flat
              ~inspect:sexp_of_resolved_db
              ~join:(fun left right -> Ok (Seq.append right left))
              ~empty:Seq.empty
              ~equal:(fun left right ->
                0 = compare_resolved_db left right)
              "resolved_db_domain"

  let seq = KB.Class.property named_symbols "seq" dom
              ~package:Common.package
  
  let unresolved_prefix = "sub_"

  let needs_resolving alert =
    match alert.sub_name with
    | None -> false
    | Some subname ->
       String.is_prefix subname ~prefix:unresolved_prefix

  let resolve_name unresolvedname db =
    let queryable_name = String.chop_prefix_if_exists unresolvedname
                           ~prefix:unresolved_prefix
                         |> String.uppercase in
    let entry_matches (addrs, name) = String.is_substring addrs
                                        ~substring:queryable_name in
    match Seq.find db ~f:entry_matches with
    | Some (_addrs, name) -> name
    | None ->
       failwith @@ sprintf "In Alert.SubNameResolverFiller.resolve_name, couldn't resolve name for symbol: %s" unresolvedname

  let resolve_sub_names alerts proj =
    let open KB.Monad_infix in
    let filename = Option.value_exn (Project.get proj filename) in
    let get_named_symbols =
      begin
        Theory.Unit.for_file filename >>= fun unit ->
        KB.collect Image.Spec.slot unit >>= fun ogre ->
        let query = Ogre.Query.(select @@ from Image.Scheme.named_symbol) in
        match Ogre.eval (Ogre.collect query) ogre with
        | Error err ->
           failwith @@ sprintf "In getting named symbols : %s" (Error.to_string_hum err)
        | Ok o ->
           let word_seq = Seq.map o ~f:(fun (addrf, name) ->
                              (Word.of_int64 addrf, name)) in
           KB.Object.create named_symbols >>= fun named_sym_obj ->
           KB.provide seq named_sym_obj word_seq >>= fun () ->
           KB.return named_sym_obj
      end in
    let named_symbols = Toplevel.eval seq get_named_symbols in
    let queryable_named_symbols = Seq.map named_symbols ~f:(fun (addr, name) ->
                                      Word.to_string addr, name) in
    (* let () = printf "queryable named symbols are:\n%!"; *)
    (*          Seq.iter queryable_named_symbols ~f:(fun (addrs, name) -> *)
    (*              printf "(%s, %s)\n%!" addrs name) in *)
    Set.map alerts ~f:(fun alert ->
        if needs_resolving alert
        then 
          let unresolved = Option.value_exn alert.sub_name in
          let resolved = resolve_name unresolved queryable_named_symbols in
          { alert with sub_name = Some resolved }
        else alert)
end

module InsnIdxFiller = struct
  let set_for_alert (idx_map : Idx_calculator.t) (alert : T.t) : T.t =
    if Idx_calculator.contains_tid alert.tid idx_map
    then
      match Idx_calculator.get_idx alert.tid idx_map with
      | Some canonical_insn_idx ->
         { alert with rpo_idx = Some canonical_insn_idx }
      | None ->
         let err_msg = sprintf "Couldn't get canonical R11 idx for insn w/ tid: %a in InsnIdxFiller.set_for_alert" Tid.pps alert.tid in
         failwith err_msg
    else
      alert
    
  let set_for_alert_set (idx_map : Idx_calculator.t) (alerts : Set.t) : Set.t =
    Set.map alerts ~f:(set_for_alert idx_map)
end

(* module InsnIdxFiller : Pass = struct *)
(*   type insn_idx = int *)

(*   type addr_string = String.t *)
  
(*   type lut = (addr_string, insn_idx, String.comparator_witness) Map.t *)

(*   type subname = String.t *)

(*   module AddrMap = struct *)
(*     include Map.Make_using_comparator(Int64) *)
(*   end *)
  
(*   type bound = (addr * addr) [@@deriving sexp, compare, bin_io] *)
  
(*   (\** (symbol_name, (start_addr, end_addr)) *\) *)
(*   type t = (string * bound) [@@deriving sexp, compare, bin_io] *)

(*   type addr_set = Core.Set.M(Word).t *)
(*   type fn_addrs = (subname, addr_set, String.comparator_witness) Map.t *)
(*   type fn_bounds = (subname, bound, String.comparator_witness) Map.t *)

(*   let cls : (t, unit) KB.cls = KB.Class.declare "symbol-starts-and-ends" () *)
(*                                  ~package:Common.package *)

(*   let dom : t Seq.t KB.domain = KB.Domain.flat "symbol-starts-and-ends-seq-domain" *)
(*                                   ~equal:(Seq.equal (fun x y -> compare x y = 0)) *)
(*                                   ~empty:Seq.empty *)

(*   let result_slot = KB.Class.property *)
(*                       cls *)
(*                       "symbol-starts-and-ends-result-slot" *)
(*                       dom *)
(*                       ~package:Common.package *)

(*   type all_addrs_idx *)
(*   let all_addrs_cls : (all_addrs_idx, unit) KB.cls = KB.Class.declare "all-addrs" () *)
(*                                                        ~package:Common.package *)
(*   let all_addrs_dom : Core.Set.M(Word).t KB.domain = KB.Domain.flat "all-addrs-domain" *)
(*                                                   ~equal:Core.Set.equal *)
(*                                                   ~empty:(Core.Set.empty (module Word)) *)
(*   let all_addrs_result_slot = KB.Class.property *)
(*                                 all_addrs_cls *)
(*                                 "all-addrs-result-slot" *)
(*                                 all_addrs_dom *)
(*                                 ~package:Common.package *)

(*   open KB.Monad_infix *)

(*   module Bound = struct *)
(*     type point = word [@@deriving compare, sexp_of] *)
    
(*     type t = point * point [@@deriving compare, sexp_of] *)

(*     let sexp_of_point = Word.sexp_of_t *)

(*     let compare_point = Word.compare *)

(*     let lower : t -> point = fst *)

(*     let upper : t -> point = snd *)
(*   end *)

(*   module IntvlTree = Interval_tree.Make(Bound) *)

(*   (\** (declare symbol-chunk (addr int) (size int) (root int)) *)
(*       (declare named-symbol (addr int) (name str)) *)
(*       (symbol-chunk 4298432 787 4298432) *)
(*       (named-symbol 4298432 argon2_initialize) *\) *)
(*   (\** Some functions have more than one symbol, like curve25519 sandy2x *)
(*       ladder which is an assembly file that has a function that starts with *)
(*       two labels: *)
(*       #define  ladder  crypto_scalarmult_curve25519_sandy2x_ladder *)
(*       #define _ladder _crypto_scalarmult_curve25519_sandy2x_ladder *)
(*       ladder: *)
(*       _ladder: *)
(*       mov %rsp,%r11 *\) *)
(*   let get_all_fn_addr_ranges proj = *)
(*     let filename = Option.value_exn (Project.get proj filename) in *)
(*     (\* let syms = Project.symbols proj |> Symtab.to_sequence in *\) *)
(*     (\* let () = Seq.iter syms ~f:(fun sym -> Symtab. *\) *)
(*     let get_all_addr_ranges = *)
(*       begin *)
(*         Theory.Unit.for_file filename >>= fun unit -> *)
(*         KB.collect Image.Spec.slot unit >>= fun ogre_doc -> *)
(*         let named_sym_q = Ogre.Query.(select @@ from Image.Scheme.named_symbol) in *)
(*         let symbol_ch_q = Ogre.Query.(select @@ from Image.Scheme.symbol_chunk) in *)
(*         let named_sym_q_res = Ogre.eval (Ogre.collect named_sym_q) ogre_doc in *)
(*         let symbol_ch_q_res = Ogre.eval (Ogre.collect symbol_ch_q) ogre_doc in *)
(*         let named_syms, symbol_chunks = *)
(*           match named_sym_q_res, symbol_ch_q_res with *)
(*           | Ok ns, Ok sc -> ns, sc *)
(*           | Error e1, Error e2 -> *)
(*              failwith @@ *)
(*                sprintf "Error running query in Alert.InsnIdxFiller: %s && %s" *)
(*                  (Error.to_string_hum e1) *)
(*                  (Error.to_string_hum e2) *)
(*           | Error e, _ *)
(*             | _, Error e -> *)
(*              failwith @@ *)
(*                sprintf "Error running query in Alert.InsnIdxFiller: %s" @@ *)
(*                  Error.to_string_hum e *)
(*         in *)
(*         (\** the addrmap can't have duplicate keys. remove all non-linked *)
(*             symbols since they all have v_addr of 0 causing an exception *)
(*             to be raised when addrmap.of_sequence_exn is built. *\) *)
(*         let symbol_sizes = Seq.map symbol_chunks *)
(*                              ~f:(fun reg -> (reg.addr, reg.size)) *)
(*         in *)
(*         let to_symbol_bound name (addr : Int64.t) (sz : Int64.t) = *)
(*           let sz = Word.of_int64 sz in *)
(*           let start_ = Word.of_int64 addr in *)
(*           let end_ = Word.(start_ + sz - (one 64)) in *)
(*           (name, (start_, end_)) *)
(*         in *)
(*         let names_with_bounds = *)
(*           Seq.map named_syms ~f:(fun (target_addr, name) -> *)
(*               Seq.fold symbol_sizes ~init:[] ~f:(fun matches (cur_addr, sz) -> *)
(*                   if Int64.equal target_addr cur_addr *)
(*                   then to_symbol_bound name target_addr sz :: matches *)
(*                   else matches) *)
(*               |> Seq.of_list) *)
(*           |> Seq.join *)
(*         in *)
(*         KB.Object.create cls >>= fun bounds_obj -> *)
(*         KB.provide result_slot bounds_obj names_with_bounds >>= fun () -> *)
(*         KB.return bounds_obj *)
(*       end  *)
(*     in *)
(*     let symbol_addr_ranges = Toplevel.eval result_slot get_all_addr_ranges in *)
(*     symbol_addr_ranges *)
(*   (\* Map.of_sequence_exn (module String) symbol_addr_ranges *\) *)

(*   let is_noop (opcode : string) : bool = *)
(*     let noop_prefixes = ["nop"; "xchg"; "fnop"] in *)
(*     (\* are any of the prefixes a caseless prefix of the given opcode? yes = true, no = false*\) *)
(*     List.exists noop_prefixes ~f:(fun prefix -> String.Caseless.is_prefix opcode ~prefix) *)

(*   let addr_of_label = KB.collect Theory.Label.addr *)

(*   let opcode_str_of_label label = *)
(*     KB.collect Theory.Semantics.slot label >>= fun insn -> *)
(*     KB.return @@ Insn.name insn *)

(*   let get_all_addrs proj : Core.Set.M(Word).t = *)
(*     let insn_used_in_calculating_indices (maddr, opcode_str) : bool KB.t = *)
(*       maddr >>= fun maddr -> *)
(*       opcode_str >>= fun opcode_str -> *)
(*       let should_use_insn = not (is_noop opcode_str) && Option.is_some maddr in *)
(*       let () = if not @@ is_noop opcode_str *)
(*                then Format.printf "Not using insn with opcode %s in calculating insn indices\n%!" opcode_str *)
(*                else () *)
(*       in *)
(*       KB.return should_use_insn *)
(*     in *)
(*     let select_addrs (maddr, _) : Bitvec.t KB.t = *)
(*       maddr >>= fun maddr -> *)
(*       KB.return @@ Option.value_exn maddr *)
(*     in *)
(*     let get_all_labels = *)
(*       begin *)
(*         KB.objects Theory.Program.cls >>= fun progs -> *)
(*         let insn_info = Seq.map progs *)
(*                           ~f:(fun label -> (addr_of_label label, opcode_str_of_label label)) *)
(*         in *)
(*         KB.Seq.filter insn_info ~f:insn_used_in_calculating_indices >>= fun filtered_insns -> *)
(*         (\* from this point, no noops are considered in calculating insn indices *\) *)
(*         KB.Seq.map filtered_insns ~f:select_addrs >>= fun all_addrs -> *)
(*         let word_addrs = Seq.map all_addrs ~f:(fun a -> *)
(*                              Word.of_int64 @@ Bitvec.to_int64 a) *)
(*                          |> Core.Set.of_sequence (module Word) in *)
(*         KB.Object.create all_addrs_cls >>= fun result_obj -> *)
(*         KB.provide all_addrs_result_slot result_obj word_addrs >>= fun () -> *)
(*         KB.return result_obj *)
(*       end *)
(*     in *)
(*     Toplevel.eval all_addrs_result_slot get_all_labels *)

(*   let symbol_bounds_to_interval_tree bounds = *)
(*     Seq.fold bounds ~init:IntvlTree.empty ~f:(fun tree (name, bound) -> *)
(*         IntvlTree.add tree bound name) *)
  
(*   let build_all_addrs_intvl_tree all_addrs bounds : word IntvlTree.t = *)
(*     Seq.fold all_addrs ~init:IntvlTree.empty ~f:(fun tree (addr : word) -> *)
(*         (\** This returns a sequence of all containing bounds *\) *)
(*         let addr_bds = IntvlTree.lookup bounds addr in *)
(*         Seq.fold addr_bds ~init:tree ~f:(fun tree (bd, _symname) -> *)
(*             IntvlTree.add tree bd addr)) *)

(*   (\** for polymorphism *\) *)
(*   let get_indices = IntvlTree.lookup *)
(*   let get_syms = IntvlTree.lookup *)

(*   let get_insn_idx addrs addr : int option = *)
(*     Option.(>>|)  *)
(*       (List.findi addrs ~f:(fun _idx -> Word.equal addr)) *)
(*       fst *)
  
(*   let alert_to_sym_and_idx alert idx_tree sym_tree : T.t = *)
(*     let addr = match alert.addr with *)
(*       | Some addr -> addr *)
(*       | None -> *)
(*          failwith "Alert addresses should be filled out before filling insn indices and symbol names" *)
(*     in *)
(*     let syms = get_syms sym_tree addr in *)
(*     let addrs = get_indices idx_tree addr in *)
(*     let sorted_addrs = List.sort (Seq.to_list addrs) ~compare:(fun l r -> *)
(*                            Word.compare (snd l) (snd r)) *)
(*                      |> List.map ~f:snd  *)
(*     in *)
(*     let sym = if Seq.length syms > 1 *)
(*               then *)
(*                 let () = printf "in Alert.InsnIdxFiller, addr %a has more than one symbol:\n%!" *)
(*                            Word.ppo addr in *)
(*                 let () = Seq.iter syms ~f:(fun (_, s) -> *)
(*                              printf "addr %a in sym %s\n%!" Word.ppo addr s) in *)
(*                 Some (snd @@ Seq.hd_exn syms) *)
(*               else *)
(*                 match Seq.hd syms with *)
(*                 | Some sym -> Some (snd @@ sym) *)
(*                 | None -> *)
(*                    let () = printf "in Alert.InsnIdxFiller, couldn't find sym for addr %a" Word.ppo addr *)
(*                    in *)
(*                    None *)
(*     in *)
(*     let insn_idx = match get_insn_idx sorted_addrs addr with *)
(*       | Some idx -> Some idx *)
(*       | None -> *)
(*          let () = printf "in Alert.InsnIdxFiller, couldn't find insn idx for addr %a in list %s" Word.ppo addr (List.to_string ~f:Word.to_string sorted_addrs) *)
(*          in *)
(*          None *)
(*     in *)
(*     { alert with sub_name = sym; *)
(*                  rpo_idx = insn_idx } *)

(*   let set_for_alert_set alerts proj = *)
(*     let symbol_addr_ranges = get_all_fn_addr_ranges proj in *)
(*     let sym_tree = symbol_bounds_to_interval_tree symbol_addr_ranges in *)
(*     let all_addrs = get_all_addrs proj |> Core.Set.to_sequence in *)
(*     let idx_tree = build_all_addrs_intvl_tree all_addrs sym_tree in *)
(*     Set.map alerts ~f:(fun alert -> *)
(*         alert_to_sym_and_idx alert idx_tree sym_tree) *)
(* end *)

module RemoveAllEmptySubName : Pass = struct
  let set_for_alert_set alerts proj =
    Set.filter alerts ~f:(fun alert -> Option.is_some alert.sub_name)
end

(** this is really more data dependency analysis like for backward slicing. 
    it was originally used for some liveness analysis, like: are any flags
    of a leaky instruction used?
    but since also adding the classical data flow liveness analysis, the
    name hasn't aged well and needs changing after paper submission *)
module LivenessFiller = struct
  type liveness = Live_variables.t

  let set_for_alert liveness alert : t =
    let warn_tid = alert.tid in
    let live_flags =
      Live_variables.get_live_flags_of_prev_def_tid liveness ~prev_def_tid:warn_tid in
    let is_live_flagless = Live_variables.is_live_flagless liveness ~tid:warn_tid in
    let is_live = is_live_flagless || not @@ SS.is_empty live_flags in
    { alert with flags_live = live_flags; is_live = Some is_live }
    
  let set_for_alert_set (alerts : Set.t) (liveness : liveness) : Set.t =
    Set.map alerts ~f:(set_for_alert liveness)
end

module DataflowLivenessFiller = struct
  let set_for_alert (liveness : Liveness.t) alert : t =
    let live_vars : SS.t = Liveness.live_at_tid alert.tid liveness in
    let flags_live_in = SS.inter live_vars AMD64SystemVABI.flag_names in
    { alert with flags_live_in = flags_live_in }

  let set_for_alert_set (alerts : Set.t) liveness : Set.t =
    Set.map alerts ~f:(set_for_alert liveness)
end

let t_of_reason_and_tid (reason : reason) (tid : Tid.t) : t =
  { sub_name = None;
    opcode = None;
    addr = None;
    rpo_idx = None;
    tid;
    reason;
    flags_live = SS.empty;
    is_live = None;
    left_val = None;
    right_val = None;
    problematic_operands = None;
    desc = "";
    flags_live_in = SS.empty
  }

let add_problematic_operands (x : t) (prob_op : int list) : t =
  match x.problematic_operands with
  | Some prev ->
     { x with problematic_operands = Some (List.append prob_op prev) }
  | None ->
     { x with problematic_operands = Some prob_op }

(* note that this prints a csv row where every field is paranoidly double quoted. rfc4180 compliant. *)
let to_string (x : t) : string =
  let { sub_name;
        opcode;
        addr;
        rpo_idx;
        tid;
        problematic_operands;
        left_val;
        right_val;
        flags_live;
        is_live;
        reason;
        desc;
        flags_live_in } = x in
  let sub_name_str = match sub_name with
    | Some sub_name -> sub_name
    | None ->
       let err_msg = Format.sprintf
                       "in Alert.to_string for alert for tid %a, by the time alerts are output, all alerts should have their corresponding subroutine name filled out."
                       Tid.pps tid in
       failwith err_msg in
  let opcode_str = Option.value opcode ~default:"" in
  let addr_str = Option.value addr ~default:(Word.one 64)
                 |> sprintf "%a" Word.pps in
  let rpo_idx_str = match rpo_idx with
    | Some i -> Int.to_string i
    | None -> "" in
  let tid_str = Tid.to_string tid in
  let po_str = match problematic_operands with
    | Some ops -> String.concat ~sep:":" @@
                    List.map ops ~f:Int.to_string
    | None -> "" in
  let left_str = Option.value left_val ~default:"" in
  let right_str = Option.value right_val ~default:"" in
  let flags_live_str = str_of_flags_live flags_live in
  let is_live_str = match is_live with
    | None -> ""
    | Some is_live -> Bool.to_string is_live in
  let reason_str = string_of_reason reason in
  let flags_live_in_str = str_of_flags_live flags_live_in in
  (* if you change this next part, then change csv_header below also *)
  sprintf
    "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\""
    sub_name_str
    opcode_str
    addr_str
    rpo_idx_str
    tid_str
    po_str
    left_str
    right_str
    flags_live_str
    is_live_str
    reason_str
    desc
    flags_live_in_str

let csv_header : string = "subroutine_name,mir_opcode,addr,rpo_idx,tid,problematic_operands,left_operand,right_operand,live_flags,is_live,alert_reason,description,flags_live_in"

let to_csv_row (alert : t) : string = (to_string alert)

let print_alert (alert : t) : unit = printf "Alert: %s\n%!" @@ to_string alert

let print_alerts : Set.t -> unit = Set.iter ~f:print_alert

let save_alerts_to_csv_file ~(filename : string) (alerts : Set.t) : unit =
  let alerts_as_csv_rows = Set.to_list alerts
                           |> List.map ~f:to_csv_row in
  let final_rows = List.cons csv_header alerts_as_csv_rows in
  Out_channel.write_lines filename final_rows

module RemoveSpuriousCompSimpAlerts : Pass = struct
  let is_comp_simp_warn alert =
    match alert.reason with
    | CompSimp -> true
    | _ -> false
  
  let is_bad_subtract_warn alert =
    let subtract_opcode_substring = "sub" in
    let cmp_opcode_substring = "cmp" in
    match alert.opcode, alert.problematic_operands with
    | None, _ -> false
    | _, None -> false
    | Some opcode, Some warn_op_indices ->
       let opcode = String.lowercase opcode in
       let is_sub = String.is_substring opcode ~substring:subtract_opcode_substring in
       let is_sub = is_sub || String.is_substring opcode ~substring:cmp_opcode_substring in
       let is_left_operand = Int.equal 0 in
       let warns_on_left_operand = List.find warn_op_indices ~f:is_left_operand
                                   |> Option.is_some
       in
       is_sub && warns_on_left_operand

  let is_bad_mov_warn alert =
    (** this also prunes any cmov instructions *)
    let mov_opcode_substring = "mov" in
    let store_opcode_substring = "stos" in
    match alert.opcode with
    | None -> false
    | Some _opcode ->
       let opcode = String.lowercase _opcode in
       let is_mov_or_cmov = String.is_substring opcode ~substring:mov_opcode_substring in
       let is_string_store = String.is_substring opcode ~substring:store_opcode_substring in
       is_mov_or_cmov || is_string_store

  (** these should really be pruned out by a good interprocedural taint analysis.
      todo, revisit this once we have the interproc taint analysis. *)
  let is_bad_stack_op alert =
    let ret = "ret" in
    let pop = "pop" in
    let push = "push" in
    match alert.opcode with
    | None -> false
    | Some _opcode ->
       let opcode = String.lowercase _opcode in
       let is_ret = String.is_substring opcode ~substring:ret in
       let is_pop = String.is_substring opcode ~substring:pop in
       let is_push = String.is_substring opcode ~substring:push in
       is_ret || is_pop || is_push

  let do_check_with_log checkname check alert : bool =
    let result = check alert in
    let () =
      if result
      then
        (printf "Comp simp spurious alert pruner (%s) pruned alert:\n%!" checkname;
         printf "%s\n%!" @@ to_string alert)
      else
        ()
    in
    result
  
  let is_spurious alert =
    do_check_with_log "SpuriousSUB" is_bad_subtract_warn alert ||
    do_check_with_log "SpuriousMOVorCMOVorSTOS" is_bad_mov_warn alert ||
    do_check_with_log "SpuriousStackOperation" is_bad_stack_op alert

  (** accept all alerts that:
      1) are not comp simp alerts OR
      2) are comp simp alerts that are not spurious *)
  let set_for_alert_set alerts proj =
    let filter_condition alert =
      (is_comp_simp_warn alert && not (is_spurious alert)) ||
        not (is_comp_simp_warn alert)
    in
    Set.filter alerts ~f:filter_condition
end
