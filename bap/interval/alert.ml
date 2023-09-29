open Core_kernel
open Bap.Std
open Common
(* open Interval_tree *)

module Theory = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

open KB.Monad_infix

type reason = CompSimp | SilentStores | Dmp | None
                           [@@deriving sexp, bin_io, compare, equal]

let string_of_reason = function
  | CompSimp -> "comp-simp"
  | SilentStores -> "silent-stores"
  | Dmp -> "dmp"
  | None -> failwith "reason should be filled out"

module T = struct
  (* flags_live: not computed or computed and true or false *)
  (* problematic_operands: doesn't apply or a list of operand indices *)
  type t = {
    sub_name : string option;
    opcode : string option;
    addr : word option;
    rpo_idx : int option;
    tid : Tid.t option;
    flags_live : SS.t;
    is_live : bool option;
    problematic_operands : int list option;
    left_val : string option;
    right_val : string option;
    reason : reason;
    desc : string;
    flags_live_in : SS.t
  } [@@deriving sexp, bin_io, compare]

  let reason_dom = KB.Domain.flat
                     ~empty:None
                     ~equal:equal_reason
                     "alert-reason-dom"

  let cls : (t, unit) KB.Class.t = KB.Class.declare
                                     ~package:Common.package
                                     "alert"
                                     ()

  let sub_name_slot = KB.Class.property
                        ~package:Common.package
                        cls
                        "sub-name"
                        Common.string_opt_domain

  let opcode_slot = KB.Class.property
                      ~package:Common.package
                      cls
                      "mir-opcode"
                      Common.string_opt_domain

  let addr_slot = KB.Class.property
                    ~package:Common.package
                    cls
                    "vaddr"
                    Common.word_opt_domain

  let rpo_idx_slot = KB.Class.property
                       ~package:Common.package
                       cls
                       "rpo-idx"
                       Common.int_opt_dom

  let tid_slot = KB.Class.property
                   ~package:Common.package
                   cls
                   "tid"
                   Common.tid_opt_domain

  let flags_live_slot = KB.Class.property
                          ~package:Common.package
                          cls
                          "flags-live-out"
                          Common.string_powset_dom

  let is_live_slot = KB.Class.property
                       ~package:Common.package
                       cls
                       "is-live"
                       Common.bool_opt_domain

  let problematic_operands_slot = KB.Class.property
                                    ~package:Common.package
                                    cls
                                    "problematic-operands"
                                    Common.int_list_opt_domain

  let left_val_slot = KB.Class.property
                        ~package:Common.package
                        cls
                        "left-val"
                        Common.string_opt_domain

  let right_val_slot = KB.Class.property
                         ~package:Common.package
                         cls
                         "right-val"
                         Common.string_opt_domain

  let reason_slot = KB.Class.property
                      ~package:Common.package
                      cls
                      "reason"
                      reason_dom

  let desc_slot = KB.Class.property
                    ~package:Common.package
                    cls
                    "desc"
                    Common.string_flat_dom

  let flags_live_in_slot = KB.Class.property
                             ~package:Common.package
                             cls
                             "flags-live-in"
                             Common.string_powset_dom

  (* as filled as a checker can fill it while
     observing the analysis, hence base *)
  let create_base_alert
        ~(tid : tid)
        ~(subname : string)
        ~(desc : string)
        ~(left_val : string)
        ~(right_val : string)
        ~(problematic_operands : int list) : unit =
    let sub_cls = KB.Class.refine cls subname in
    Toplevel.exec begin
      KB.Object.create sub_cls >>= fun obj ->
      KB.all_unit [
        KB.provide tid_slot obj (Some tid);
        KB.provide sub_name_slot obj (Some subname);
        KB.provide desc_slot obj desc;
        KB.provide left_val_slot obj (Some left_val);
        KB.provide right_val_slot obj (Some right_val);
        KB.provide problematic_operands_slot obj (Some problematic_operands)
      ]
    end

  let reify kbalert : t =
    let alert : t ref = ref { sub_name = None;
                              opcode = None;
                              addr = None;
                              rpo_idx = None;
                              tid = None;
                              flags_live = SS.empty;
                              is_live = None;
                              problematic_operands = None;
                              left_val = None;
                              right_val = None;
                              reason = None;
                              desc = "";
                              flags_live_in = SS.empty }
    in
    let () = Toplevel.exec begin
      KB.collect tid_slot kbalert >>= fun tid ->
      KB.collect sub_name_slot kbalert >>= fun sub_name ->
      KB.collect opcode_slot kbalert >>= fun opcode ->
      KB.collect addr_slot kbalert >>= fun addr ->
      KB.collect rpo_idx_slot kbalert >>= fun rpo_idx ->
      KB.collect flags_live_slot kbalert >>= fun flags_live ->
      KB.collect is_live_slot kbalert >>= fun is_live ->
      KB.collect problematic_operands_slot kbalert >>= fun problematic_operands ->
      KB.collect left_val_slot kbalert >>= fun left_val ->
      KB.collect right_val_slot kbalert >>= fun right_val ->
      KB.collect reason_slot kbalert >>= fun reason ->
      KB.collect desc_slot kbalert >>= fun desc ->
      KB.collect flags_live_in_slot kbalert >>= fun flags_live_in ->
      alert := {
        tid;
        sub_name;
        opcode;
        addr;
        rpo_idx;
        flags_live;
        is_live;
        problematic_operands;
        left_val;
        right_val;
        reason;
        desc;
        flags_live_in
      };
      KB.return ()
    end
    in
    !alert
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
        let opcode_str = Insn.name sema in
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
    let alert_tid = Option.value_exn alert.tid in
    match Map.find addr_lut alert_tid with
    | Some addr_str -> 
      { alert with addr = Some addr_str }
    | None ->
      failwith @@ sprintf "In OpcodeAndAddrFiller.build_lut, set_addr_for_alert, couldn't get addr for alert on tid: %a" Tid.pps alert_tid

  let set_opcode_for_alert proj (alert : T.t) (opcode_lut : tidmap) : T.t =
    let alert_tid = Option.value_exn alert.tid in
    let opcode_name = ref "" in
    let addr_str = ref "" in
    let target = Project.target proj in
    let () =
      Toplevel.exec begin        
        KB.collect Theory.Semantics.slot alert_tid >>= fun sema ->
        KB.collect Theory.Label.addr alert_tid >>= fun maybe_addr ->
        let maybe_addr_bitvector =
          Option.bind maybe_addr ~f:(fun bitv ->
            Option.return @@ Bitvector.code_addr target bitv)
        in
        addr_str := Word.to_string @@
          Option.value maybe_addr_bitvector ~default:(Word.one 64);
        opcode_name := Insn.name sema;
        KB.return ()
      end
    in
    match Map.find opcode_lut alert_tid with
    | Some opcode_str -> 
      { alert with opcode = Some opcode_str }
    | None ->
      failwith @@ sprintf "In OpcodeAndAddrFiller.build_lut, set_opcode_for_alert, couldn't get opcode for alert on tid: %a" Tid.pps alert_tid

  let set_for_alert_set (alerts : Set.t) (proj : Project.t) : Set.t =
    let lut = build_lut proj in
    let (addr_lut, opcode_lut) = lut in
    Set.map alerts ~f:(fun alert ->
      let alert = set_addr_for_alert alert addr_lut in
      set_opcode_for_alert proj alert opcode_lut)
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

  let has_unres_prefix =
    String.Caseless.is_prefix ~prefix:unresolved_prefix

  let get_name_for_resolving alert =
    Option.bind alert.sub_name ~f:(fun name ->
      if has_unres_prefix name
      then Some name
      else None)

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
    | Some (_addrs, name) -> Some name
    | None ->
      let () = printf "In Alert.SubNameResolverFiller.resolve_name, couldn't resolve name for symbol: %s" unresolvedname in
      None
  (* failwith @@  *)

  let resolve_sub_names alerts proj =
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
    Set.map alerts ~f:(fun alert ->
      if needs_resolving alert
      then 
        let unresolved = Option.value_exn alert.sub_name in
        let resolved = resolve_name unresolved queryable_named_symbols in
        let resolved = match resolved with
          | Some name -> Some name
          | None -> Some unresolved in
        { alert with sub_name = resolved }
      else alert)

  let set_for_alert_set alerts proj =
    let open Option.Monad_infix in
    let set_for_alert aliasmap a =
      match a.sub_name with
      | None -> a
      | Some name ->
        if not (has_unres_prefix name)
        then a
        else
          let maliases = List.Assoc.find aliasmap name
                           ~equal:String.equal in
          match maliases with
          | Some aliases when 0 = SS.length aliases ->
            printf "[Alert] Couldn't resolve subname for %s" name;
            a
          | None ->
            printf "[Alert] Couldn't resolve subname for %s" name;
            a
          | Some aliases when SS.length aliases > 0 ->
            let newname = SS.elements aliases |> List.hd_exn in
            let () = if SS.length aliases > 1
              then begin
                printf "[Alert] multiple sub name candidates for sub %s:\n%!" name;
                SS.iter aliases ~f:(printf "\t%s\n%!");
                printf "[Alert] picking first alias: %s\n%!" newname end
              else
                printf "[Alert] resolved %s to %s\n%!" name newname in
            { a with sub_name = Some newname }
          | _ -> failwith "impossible"
    in
    let toresolve = Set.fold alerts ~init:SS.empty
                      ~f:(fun toresolve a ->
                        match get_name_for_resolving a with
                        | Some n -> SS.add toresolve n
                        | None -> toresolve) in
    let () = printf "[Alerts] trying to resolve sub names:\n%!";
      SS.iter toresolve ~f:(printf "\t%s\n%!") in
    let symbol_aliases = Config.get_all_named_symbols proj toresolve in
    let () = printf "[Config] resolved is:\n%!";
      List.iter symbol_aliases ~f:(fun (sym, aliases) ->
        printf "\t%s ~~> %s\n%!" sym @@
        (SS.to_list aliases |> List.to_string ~f:(fun x -> x))) in
    Set.map alerts ~f:(set_for_alert symbol_aliases)
end

module InsnIdxFiller = struct
  let set_for_alert (idx_map : Idx_calculator.t) (alert : T.t) : T.t =
    let tid = Option.value_exn alert.tid in
    if Idx_calculator.contains_tid tid idx_map
    then
      match Idx_calculator.get_idx tid idx_map with
      | Some canonical_insn_idx ->
        { alert with rpo_idx = Some canonical_insn_idx }
      | None ->
        let err_msg = sprintf "Couldn't get canonical R11 idx for insn w/ tid: %a in InsnIdxFiller.set_for_alert" Tid.pps tid in
        failwith err_msg
    else
      alert

  let set_for_alert_set (idx_map : Idx_calculator.t) (alerts : Set.t) : Set.t =
    Set.map alerts ~f:(set_for_alert idx_map)
end

module RemoveAllEmptySubName : Pass = struct
  let set_for_alert_set alerts proj =
    Set.iter alerts ~f:(fun a ->
      match a.sub_name with
      | None ->
        let tid = Option.value_exn a.tid in
        Format.sprintf "[Alert] alert for %a had empty subname"
          Tid.pps tid
        |> failwith
      | Some _ -> ());
    alerts
    (* Set.filter alerts ~f:(fun alert -> Option.is_some alert.sub_name) *)
end

module FlagsLiveOutFiller = struct
  let set_for_alert tidmap flagownership rd alert : T.t =
    let flagname tid =
      match Tid_map.find tidmap tid with
      | Some (`Def d) -> Var.name (Def.lhs d)
      | _ -> failwith "FlagLiveOutFiller set_for_alert error"
    in
    let tid = Option.value_exn alert.tid in
    let flags = Tid_map.find flagownership tid in
    let flags_live = match flags with
      | None -> SS.empty
      | Some flags ->
        Core_kernel.Set.fold flags ~init:SS.empty ~f:(fun liveflags flagdeftid ->
          if Reachingdefs.has_users rd flagdeftid
          then SS.add liveflags @@ flagname flagdeftid
          else liveflags) in
    { alert with flags_live }

  let set_for_alert_set tidmap flagownership depanalysis alerts : Set.t =
    Set.map alerts ~f:(set_for_alert tidmap flagownership depanalysis)
end

module DataflowLivenessFiller = struct
  let set_for_alert liveness alert =
    let tid = Option.value_exn alert.tid in
    let live_vars : SS.t = Liveness.liveness_at_tid liveness tid in
    let flags_live_in = SS.inter live_vars AMD64SystemVABI.flag_names in
    { alert with flags_live_in = flags_live_in }

  let set_for_alert_set alerts liveness =
    Set.map alerts ~f:(set_for_alert liveness)
end

let t_of_reason_and_tid (reason : reason) (tid : Tid.t) : t =
  { sub_name = None;
    opcode = None;
    addr = None;
    rpo_idx = None;
    tid = Some tid;
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
        flags_live_in } = x
  in
  let tid = Option.value_exn tid in
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

module RemoveAndWarnEmptyInsnIdxAlerts : Pass = struct
  let warn_on_no_insn_idx alert : unit =
    let tid = Option.value_exn alert.tid in
    let csv_row = to_csv_row alert in 
    printf "[Alert] (%a) has empty insn idx. Full alert csv row is: %s\n%!"
      Tid.ppo tid csv_row

  let has_insn_idx alert : bool =
    match alert.rpo_idx with
    | Some _ -> true
    | None ->
      let () = warn_on_no_insn_idx alert in
      false

  let set_for_alert_set alerts _proj =
    Set.filter alerts ~f:has_insn_idx
end

(* these are not supported by us right now *)
module RemoveAlertsForCallInsns : Pass = struct
  let call_insn_prefix = "call"

  let is_alert_on_call_insn alert : bool =
    match alert.opcode with
    | Some opcode_str ->
      String.Caseless.is_prefix opcode_str ~prefix:call_insn_prefix
    | None -> false

  let set_for_alert_set alerts _proj =
    let is_not_call_alert = fun alert -> not @@ is_alert_on_call_insn alert in
    Set.filter alerts ~f:is_not_call_alert
end

module RemoveUnsupportedMirOpcodes = struct
  type t = {
    num_removed : int;
    alerts : Set.t;
  }

  (* prefix is checked case insensitive *)
  let unsupported_mir_opcode_prefixes = ["xorps";
                                         "adc";
                                         "shld";
                                         "shrd";
                                         "sbb";
                                         "psub";
                                         "psrl";
                                         "punpck";
                                         "psll";
                                         "por";
                                         "psbu";
                                         "pand";
                                         "pxor";
                                         "pshuf";
                                         "shrd";
                                         "rol";
                                         "ror";
                                         "div"]

  let is_unsupported : T.t -> bool = function
    | { opcode = Some mir_opcode_str; _ } ->
      List.exists unsupported_mir_opcode_prefixes ~f:(fun prefix ->
        String.Caseless.is_prefix mir_opcode_str ~prefix)
    | _ -> false

  let set_for_alert_set alerts _proj : t =
    let unsupported_count = ref 0 in
    let is_supported = fun alert ->
      let not_supported = is_unsupported alert in
      let () = if not_supported
        then
          let () = unsupported_count := !unsupported_count + 1 in
          let alert_s = to_string alert in
          printf "[AlertFilter] alert (%s) removed since it is for unsupported insn\n%!" alert_s
        else () in
      not not_supported in
    { alerts = Set.filter alerts ~f:is_supported;
      num_removed = !unsupported_count }
end

module CombinedTransformFixerUpper : Pass = struct
  (** this alert transform pass changes CS warns
      on non-store opcodes whose destination is memory 
      to SS warns since these combined CS+SS transforms
      are in the SS pass *)
  let combined_transforms_opcodes = ["add64mr";
                                     "xor64mr";
                                     "add64mi32";
                                     "add32mi8";
                                     "add64mi8";
                                     "add8mr";
                                     "add32mr";
                                     "and8mi";
                                     "and32mr"]

  let is_combined_opcode (mir_opcode : string) : bool =
    let equal = String.Caseless.equal in
    List.mem combined_transforms_opcodes mir_opcode ~equal

  let transform_alert alert =
    match alert.opcode with
    | Some opcode -> if is_combined_opcode opcode
      then { alert with reason = SilentStores }
      else alert
    | None -> alert

  let set_for_alert_set alerts _proj =
    Set.map alerts ~f:transform_alert
end

module RemoveSpuriousCompSimpAlerts = struct
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

  (* rotate is a (x << amount) | (x >> amount') operation. as long
     as the shifts are safe, then the rotate is safe. it shouldn't be 
     expected that the OR to combine the two is CS safe. *)
  let is_false_rotate_warn alert =
    let rol_opcode_prefix = "rol" in
    let ror_opcode_prefix = "ror" in
    match alert.opcode with
    | Some mir_opcode ->
      (String.Caseless.is_prefix mir_opcode ~prefix:ror_opcode_prefix ||
       String.Caseless.is_prefix mir_opcode ~prefix:rol_opcode_prefix) &&
      String.equal "|" alert.desc
    | _ -> false

  let is_bad_mov_warn alert =
    (** this also prunes any cmov instructions *)
    let mov_opcode_substring = "mov" in
    let store_opcode_substring = "stos" in
    match alert.opcode with
    | None -> false
    | Some opcode ->
      let opcode = String.lowercase opcode in
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
    do_check_with_log "SpuriousStackOperation" is_bad_stack_op alert ||
    do_check_with_log "SpuriousROLorRORbinaryORwwarn" is_false_rotate_warn alert

  (** accept all alerts that:
      1) are not comp simp alerts OR
      2) are comp simp alerts that are not spurious *)
  let set_for_alert_set alerts proj : Set.t =
    let filter_condition alert =
      (is_comp_simp_warn alert && not (is_spurious alert)) ||
      not (is_comp_simp_warn alert) in
    Set.filter alerts ~f:filter_condition
end
