open Core
open Bap.Std
open Common

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
             addr : string option;
             rpo_idx : int option;
             tid : Tid.t;
             flags_live : SS.t;
             is_live : bool option;
             problematic_operands : int list option;
             left_val : string option;
             right_val : string option;
             reason : reason;
             desc : string }
             [@@deriving sexp, bin_io, compare, equal]
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
  
  type addr_alist = (tid * String.t) list [@@deriving compare, sexp, bin_io]

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

  type map = (Tid.t, string, Tid.comparator_witness) Map.t
  type lut = map * map

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
                 let maybe_addr_bitvector = Option.bind maybe_addr ~f:(fun addr ->
                                                          let addr = Bitvector.code_addr target addr in
                                                          Some (Bitvector.to_string addr)) in
                 let addr_str = Option.value maybe_addr_bitvector ~default:"" in
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

  let set_addr_for_alert (alert : T.t) (addr_lut : map) : T.t =
    let alert_tid = alert.tid in
    match Map.find addr_lut alert_tid with
    | Some addr_str -> 
       { alert with addr = Some addr_str }
    | None ->
       failwith @@ sprintf "In OpcodeAndAddrFiller.build_lut, set_addr_for_alert, couldn't get addr for alert on tid: %a" Tid.pps alert_tid

  let set_opcode_for_alert (alert : T.t) (opcode_lut : map) : T.t =
    let alert_tid = alert.tid in
    match Map.find opcode_lut alert_tid with
    | Some opcode_str -> 
       { alert with opcode = Some opcode_str }
    | None ->
       failwith @@ sprintf "In OpcodeAndAddrFiller.build_lut, set_opcode_for_alert, couldn't get opcode for alert on tid: %a" Tid.pps alert_tid
                                            
  let set_opcode_and_addr_for_alert_set (alerts : Set.t) (proj : Project.t) : Set.t =
    let lut : lut = build_lut proj in
    let (addr_lut, opcode_lut) = lut in
    Set.map alerts ~f:(fun alert -> set_addr_for_alert alert addr_lut)
    |> Set.map ~f:(fun alert -> set_opcode_for_alert alert opcode_lut)
end

(* RPO = reverse post-order traversal *)
module RpoIdxAlertFiller = struct
  let set_rpo_idx_for_alert (alert : T.t) (rpo_addrs : string list) : T.t =
    let this_addr : string = match alert.addr with
      | Some addr_str -> addr_str
      | None -> failwith "In RpoIdxAlertFiller.set_rpo_idx_for_alert, alerts should have addr strings filled out before filling out rpo indices" in
    let rpo_idx = match List.findi rpo_addrs ~f:(fun _idx elt ->
                                           String.equal elt this_addr) with
      | Some (rpo_idx, _elt) -> rpo_idx
      | None -> failwith @@ sprintf "In RpoIdxAlertFiller.set_rpo_idx_for_alert, didn't find addr %s, probably mismatch in alert vs sub" this_addr in
    { alert with rpo_idx = Some rpo_idx }

  let set_rpo_indices_for_alert_set (alerts : Set.t)
                                    (sub : sub term)
                                    (proj : Project.t)
                                    (rpo_traversal : Calling_context.t Sequence.t) : Set.t =
    let () = printf "Setting rpo indices for sub %s\n%!" @@ Sub.name sub in
    let rpo_tids = Seq.map rpo_traversal ~f:Calling_context.to_insn_tid in
    let () = printf "Rpo tids are:\n%!";
             Seq.iter rpo_tids ~f:(fun tid -> printf "%a\n%!" Tid.ppo tid) in
    let lut = OpcodeAndAddrFiller.build_lut proj in
    let (addr_lut, _) = lut in
    (* let () = printf "addr_lut is:\n%!"; *)
    (*          Map.iteri addr_lut ~f:(fun ~key ~data -> *)
    (*                      printf "\t%a : %s\n%!" Tid.ppo key data) in *)
    let addr_of_tid_exn tid : string = match Map.find addr_lut tid with
      | Some tid -> tid
      | None ->
         failwith @@
           sprintf "In RpoIdxAlertFiller.set_rpo_indices_for_alert_set, couldn't find addr in addr_lut for tid %a"
                   Tid.pps tid in
    let rpo_addrs = Seq.map rpo_tids ~f:addr_of_tid_exn in
    let grouped_rpo_addrs = Seq.to_list rpo_addrs
                            |> List.group ~break:String.(<>) in
    let rpo_addrs = List.map grouped_rpo_addrs ~f:List.hd_exn in
    Set.map alerts ~f:(fun a -> set_rpo_idx_for_alert a rpo_addrs)
end

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
  let addr_str = Option.value addr ~default:"" in
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
