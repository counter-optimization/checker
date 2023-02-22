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
  type t = { sub_name : string option;
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
             desc : string }
             [@@deriving sexp, bin_io, compare]
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
  

module OpcodeAndAddrFiller = struct
  (* let set_opcode_for_alert (alert : T.t) : T.t = *)
  (*   let st = Toplevel.current () in *)
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
  type insn_idx = int

  type addr_string = String.t
  
  type lut = (addr_string, insn_idx, String.comparator_witness) Map.t

  type subname = String.t

  module AddrMap = struct
    include Map.Make_using_comparator(Int64)
  end
  
  type bound = (addr * addr) [@@deriving sexp, compare, bin_io]
  
  (** (symbol_name, (start_addr, end_addr)) *)
  type t = (string * bound) [@@deriving sexp, compare, bin_io]

  type addr_set = Core.Set.M(Word).t
  type fn_addrs = (subname, addr_set, String.comparator_witness) Map.t
  type fn_bounds = (subname, bound, String.comparator_witness) Map.t

  let cls : (t, unit) KB.cls = KB.Class.declare "symbol-starts-and-ends" ()
                                 ~package:Common.package

  let dom : t Seq.t KB.domain = KB.Domain.flat "symbol-starts-and-ends-seq-domain"
                                  ~equal:(Seq.equal (fun x y -> compare x y = 0))
                                  ~empty:Seq.empty

  let result_slot = KB.Class.property
                      cls
                      "symbol-starts-and-ends-result-slot"
                      dom
                      ~package:Common.package

  type all_addrs_idx
  let all_addrs_cls : (all_addrs_idx, unit) KB.cls = KB.Class.declare "all-addrs" ()
                                                       ~package:Common.package
  let all_addrs_dom : Core.Set.M(Word).t KB.domain = KB.Domain.flat "all-addrs-domain"
                                                  ~equal:Core.Set.equal
                                                  ~empty:(Core.Set.empty (module Word))
  let all_addrs_result_slot = KB.Class.property
                                all_addrs_cls
                                "all-addrs-result-slot"
                                all_addrs_dom
                                ~package:Common.package

  open KB.Monad_infix

  module Bound = struct
    type point = word [@@deriving compare, sexp_of]
    
    type t = point * point [@@deriving compare, sexp_of]

    let sexp_of_point = Word.sexp_of_t

    let compare_point = Word.compare

    let lower : t -> point = fst

    let upper : t -> point = snd
  end

  module IntvlTree = Interval_tree.Make(Bound)

  (** (declare symbol-chunk (addr int) (size int) (root int))
      (declare named-symbol (addr int) (name str))
      (declare named-symbol (addr int) (name str))
      (symbol-chunk 4298432 787 4298432)
      (named-symbol 4298432 argon2_initialize) *)
  (** Some functions have more than one symbol, like curve25519 sandy2x
      ladder which is an assembly file that has a function that starts with
      two labels:
      #define  ladder  crypto_scalarmult_curve25519_sandy2x_ladder
      #define _ladder _crypto_scalarmult_curve25519_sandy2x_ladder
      ladder:
      _ladder:
      mov %rsp,%r11 *)
  let get_all_fn_addr_ranges proj =
    let filename = Option.value_exn (Project.get proj filename) in
    (* let syms = Project.symbols proj |> Symtab.to_sequence in *)
    (* let () = Seq.iter syms ~f:(fun sym -> Symtab. *)
    let get_all_addr_ranges =
      begin
        Theory.Unit.for_file filename >>= fun unit ->
        KB.collect Image.Spec.slot unit >>= fun ogre_doc ->
        let named_sym_q = Ogre.Query.(select @@ from Image.Scheme.named_symbol) in
        let symbol_ch_q = Ogre.Query.(select @@ from Image.Scheme.symbol_chunk) in
        let named_sym_q_res = Ogre.eval (Ogre.collect named_sym_q) ogre_doc in
        let symbol_ch_q_res = Ogre.eval (Ogre.collect symbol_ch_q) ogre_doc in
        let named_syms, symbol_chunks =
          match named_sym_q_res, symbol_ch_q_res with
          | Ok ns, Ok sc -> ns, sc
          | Error e1, Error e2 ->
             failwith @@
               sprintf "Error running query in Alert.InsnIdxFiller: %s && %s"
                 (Error.to_string_hum e1)
                 (Error.to_string_hum e2)
          | Error e, _
            | _, Error e ->
             failwith @@
               sprintf "Error running query in Alert.InsnIdxFiller: %s" @@
                 Error.to_string_hum e
        in
        (** the addrmap can't have duplicate keys. remove all non-linked
            symbols since they all have v_addr of 0 causing an exception
            to be raised when addrmap.of_sequence_exn is built. *)
        let symbol_sizes = Seq.map symbol_chunks
                             ~f:(fun reg -> (reg.addr, reg.size))
        in
        let to_symbol_bound name (addr : Int64.t) (sz : Int64.t) =
          let sz = Word.of_int64 sz in
          let start_ = Word.of_int64 addr in
          let end_ = Word.(start_ + sz - (one 64)) in
          (name, (start_, end_))
        in
        let names_with_bounds =
          Seq.map named_syms ~f:(fun (target_addr, name) ->
              Seq.fold symbol_sizes ~init:[] ~f:(fun matches (cur_addr, sz) ->
                  if Int64.equal target_addr cur_addr
                  then to_symbol_bound name target_addr sz :: matches
                  else matches)
              |> Seq.of_list)
          |> Seq.join
        in
        KB.Object.create cls >>= fun bounds_obj ->
        KB.provide result_slot bounds_obj names_with_bounds >>= fun () ->
        KB.return bounds_obj
      end 
    in
    let symbol_addr_ranges = Toplevel.eval result_slot get_all_addr_ranges in
    symbol_addr_ranges
    (* Map.of_sequence_exn (module String) symbol_addr_ranges *)

  let get_all_addrs proj : Core.Set.M(Word).t =
    let get_all_labels =
      begin
        KB.objects Theory.Program.cls >>= fun progs ->
        let maddrs =
          Seq.map progs ~f:(KB.collect Theory.Label.addr) in
        KB.Seq.filter maddrs
          ~f:(fun maddr -> maddr >>| Option.is_some) >>= fun filtered_addrs ->
        KB.Seq.map filtered_addrs ~f:(fun maddr ->
            maddr >>| (fun o -> Option.value_exn o)) >>= fun all_addrs ->
        let word_addrs = Seq.map all_addrs ~f:(fun a ->
                             Word.of_int64 @@ Bitvec.to_int64 a)
                         |> Core.Set.of_sequence (module Word) in
        KB.Object.create all_addrs_cls >>= fun result_obj ->
        KB.provide all_addrs_result_slot result_obj word_addrs >>= fun () ->
        KB.return result_obj
      end
    in
    Toplevel.eval all_addrs_result_slot get_all_labels

  let symbol_bounds_to_interval_tree bounds =
    Seq.fold bounds ~init:IntvlTree.empty ~f:(fun tree (name, bound) ->
        IntvlTree.add tree bound name)
  
  let build_all_addrs_intvl_tree all_addrs bounds : word IntvlTree.t =
    Seq.fold all_addrs ~init:IntvlTree.empty ~f:(fun tree (addr : word) ->
        (** This returns a sequence of all containing bounds *)
        let addr_bds = IntvlTree.lookup bounds addr in
        Seq.fold addr_bds ~init:tree ~f:(fun tree (bd, _symname) ->
            IntvlTree.add tree bd addr))

  (** for polymorphism *)
  let get_indices = IntvlTree.lookup
  let get_syms = IntvlTree.lookup

  let get_insn_idx addrs addr : int option =
    Option.(>>|) 
      (List.findi addrs ~f:(fun _idx -> Word.equal addr))
      fst
  
  let alert_to_sym_and_idx alert idx_tree sym_tree : T.t =
    let addr = match alert.addr with
      | Some addr -> addr
      | None ->
         failwith "Alert addresses should be filled out before filling insn indices and symbol names"
    in
    let syms = get_syms sym_tree addr in
    let addrs = get_indices idx_tree addr in
    let sorted_addrs = List.sort (Seq.to_list addrs) ~compare:(fun l r ->
                           Word.compare (snd l) (snd r))
                     |> List.map ~f:snd 
    in
    let () = Seq.iter syms ~f:(fun (_, s) ->
                 printf "addr %a in sym %s\n%!" Word.ppo addr s) in
    (* let () = List.iter sorted_addrs ~f:(fun i -> *)
    (*              printf "addr %a has addrs %a\n%!" Word.ppo addr Word.ppo i) in *)
    let sym = if Seq.length syms > 1
              then
                let () = printf "in Alert.InsnIdxFiller, addr %a has more than one symbol:\n%!"
                           Word.ppo addr in
                let () = Seq.iter syms ~f:(fun (_, s) ->
                             printf "addr %a in sym %s\n%!" Word.ppo addr s) in
                Some (snd @@ Seq.hd_exn syms)
              else
                match Seq.hd syms with
                | Some sym -> Some (snd @@ sym)
                | None ->
                   let () = printf "in Alert.InsnIdxFiller, couldn't find sym for addr %a" Word.ppo addr
                   in
                   None
    in
    let insn_idx = match get_insn_idx sorted_addrs addr with
      | Some idx -> Some idx
      | None ->
         let () = printf "in Alert.InsnIdxFiller, couldn't find insn idx for addr %a in list %s" Word.ppo addr (List.to_string ~f:Word.to_string sorted_addrs)
         in
         None
    in
    { alert with sub_name = sym;
                 rpo_idx = insn_idx }

  let set_for_alert_set alerts proj =
    let symbol_addr_ranges = get_all_fn_addr_ranges proj in
    let sym_tree = symbol_bounds_to_interval_tree symbol_addr_ranges in
    let all_addrs = get_all_addrs proj |> Core.Set.to_sequence in
    let idx_tree = build_all_addrs_intvl_tree all_addrs sym_tree in
    Set.map alerts ~f:(fun alert ->
        alert_to_sym_and_idx alert idx_tree sym_tree)
end

module RemoveAllEmptySubName = struct
  let set_for_alert_set alerts proj =
    Set.filter alerts ~f:(fun alert -> Option.is_some alert.sub_name)
end

(* RPO = reverse post-order traversal *)
(* module RpoIdxAlertFiller = struct *)

(*   type lut = (Tid.t, string, Tid.comparator_witness) Map.t *)
  
(*   let set_rpo_idx_for_alert (alert : T.t) (rpo_addrs : string list) : T.t = *)
(*     let this_addr : string = match alert.addr with *)
(*       | Some addr_str -> addr_str *)
(*       | None -> failwith "In RpoIdxAlertFiller.set_rpo_idx_for_alert, alerts should have addr strings filled out before filling out rpo indices" in *)
(*     let rpo_idx = match List.findi rpo_addrs ~f:(fun _idx elt -> *)
(*                                            String.equal elt this_addr) with *)
(*       | Some (rpo_idx, _elt) -> rpo_idx *)
(*       | None -> failwith @@ sprintf "In RpoIdxAlertFiller.set_rpo_idx_for_alert, didn't find addr %s, probably mismatch in alert vs sub" this_addr in *)
(*     { alert with rpo_idx = Some rpo_idx } *)

(*   let build_lut_for_sub sub : lut = *)
(*     let blks = Term.enum blk_t sub in *)
(*     let defs = Seq.map blks ~f:(Term.enum def_t) |> Seq.join in *)
(*     let jmps = Seq.map blks ~f:(Term.enum jmp_t) |> Seq.join in *)
(*     let phis = Seq.map blks ~f:(Term.enum phi_t) |> Seq.join in *)
(*     let tid_and_addr_of_term : 'a. 'a term -> (tid * string) option = fun t -> *)
(*       let tid = Term.tid t in *)
(*       begin *)
(*         match Term.get_attr t address with *)
(*         | Some addr -> Some addr *)
(*         | None -> *)
(*            let () = printf "In RpoIdxAlertFiller.build_lut_for_sub, didn't find addr for tid %a" Tid.ppo tid in *)
(*            None *)
(*       end *)
(*       |> Option.bind ~f:(fun addr -> *)
(*              let addr_str = Word.to_string addr in *)
(*              Some (tid, addr_str)) in *)
(*     let def_alist = Seq.map defs ~f:tid_and_addr_of_term in *)
(*     let jmp_alist = Seq.map jmps ~f:tid_and_addr_of_term in *)
(*     let phi_alist = Seq.map phis ~f:tid_and_addr_of_term in *)
(*     let all_alist = Seq.append def_alist jmp_alist *)
(*                     |> Seq.append phi_alist *)
(*                     |> Seq.filter ~f:Option.is_some *)
(*                     |> Seq.map ~f:(fun p -> Option.value_exn p) *)
(*                     |> Seq.to_list in *)
(*     Map.of_alist_exn (module Tid) all_alist *)

(*   let set_rpo_indices_for_alert_set (alerts : Set.t) *)
(*                                     (sub : sub term) *)
(*                                     (proj : Project.t) *)
(*                                     (rpo_traversal : Calling_context.t Sequence.t) : Set.t = *)
(*     (\* let () = printf "Setting rpo indices for sub %s\n%!" @@ Sub.name sub in *\) *)
(*     let rpo_tids = Seq.map rpo_traversal ~f:Calling_context.to_insn_tid in *)
(*     (\* let () = printf "Rpo tids are:\n%!"; *\) *)
(*     (\*          Seq.iter rpo_tids ~f:(fun tid -> printf "%a\n%!" Tid.ppo tid) in *\) *)
(*     (\* let lut = OpcodeAndAddrFiller.build_lut proj in *\) *)
(*     (\* let (addr_lut, _) = lut in *\) *)
(*     let addr_lut = build_lut_for_sub sub in *)
(*     (\* let () = printf "addr_lut is:\n%!"; *\) *)
(*     (\*          Map.iteri addr_lut ~f:(fun ~key ~data -> *\) *)
(*     (\*                      printf "\t%a : %s\n%!" Tid.ppo key data) in *\) *)
(*     let addr_of_tid_exn tid : string option = match Map.find addr_lut tid with *)
(*       | Some tid -> Some tid *)
(*       | None -> *)
(*          let () = printf "In RpoIdxAlertFiller.set_rpo_indices_for_alert_set, couldn't find addr in addr_lut for tid %a" *)
(*                     Tid.ppo tid in *)
(*          None in *)
(*     let rpo_addrs = Seq.map rpo_tids ~f:addr_of_tid_exn in *)
(*     let grouped_rpo_addrs = Seq.to_list rpo_addrs *)
(*                             |> List.filter ~f:Option.is_some *)
(*                             |> List.map ~f:(fun mrpo -> Option.value_exn mrpo) *)
(*                             |> List.group ~break:String.(<>) in *)
(*     let rpo_addrs = List.map grouped_rpo_addrs ~f:List.hd_exn in *)
(*     Set.map alerts ~f:(fun a -> set_rpo_idx_for_alert a rpo_addrs) *)
(* end *)

module LivenessFiller = struct
  type liveness = Live_variables.t

  let set_for_alert (alert : t) (liveness : liveness) : t =
    let warn_tid = alert.tid in

    (* let users : (string * tid) list = *)
    (*   Live_variables.get_users_names_and_tids liveness ~of_tid:warn_tid in *)
    
    (* let flag_users : (string * tid) list = *)
    (*   List.filter users ~f:(fun (name, tid) -> Common.var_name_is_x86_64_flag name) in *)

    let live_flags =
      Live_variables.get_live_flags_of_prev_def_tid liveness ~prev_def_tid:warn_tid in

    let is_live_flagless = Live_variables.is_live_flagless liveness ~tid:warn_tid in

    let is_live = is_live_flagless || not @@ SS.is_empty live_flags in
    
    (* let flag_users_names : SS.t = List.map flag_users ~f:fst |> SS.of_list in *)
    
    (* let is_live = not @@ List.is_empty users in *)
    
    { alert with flags_live = live_flags; is_live = Some is_live }
    
  let set_for_alert_set (alerts : Set.t) (liveness : liveness) : Set.t =
    Set.map alerts ~f:(fun single_alert -> set_for_alert single_alert liveness)
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
    desc = ""
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
        desc } = x in
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
  (* if you change this next part, then change csv_header below also *)
  sprintf
    "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\""
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

let csv_header : string = "subroutine_name,mir_opcode,addr,rpo_idx,tid,problematic_operands,left_operand,right_operand,live_flags,is_live,alert_reason,description"

let to_csv_row (alert : t) : string = (to_string alert)

let print_alert (alert : t) : unit = printf "Alert: %s\n%!" @@ to_string alert

let print_alerts : Set.t -> unit = Set.iter ~f:print_alert

let save_alerts_to_csv_file ~(filename : string) (alerts : Set.t) : unit =
  let alerts_as_csv_rows = Set.to_list alerts
                           |> List.map ~f:to_csv_row in
  let final_rows = List.cons csv_header alerts_as_csv_rows in
  Out_channel.write_lines filename final_rows
