open Core_kernel
open Bap.Std

type reason = CompSimp | SilentStores
                           [@@deriving sexp, bin_io, compare, equal]

let string_of_reason = function
  | CompSimp -> "comp-simp"
  | SilentStores -> "silent-stores"

module T = struct
  (* flags_live: not computed or computed and true or false *)
  (* problematic_operands: doesn't apply or a list of operand indices *)
  type t = { tid : Tid.t;
             flags_live : bool option;
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

let t_of_reason_and_tid (reason : reason) (tid : Tid.t) : t =
  { tid;
    reason;
    flags_live = None;
    left_val = None;
    right_val = None;
    problematic_operands = None;
    desc = ""
  }

let set_flags_live (x : t) : t =
  { x with flags_live = Some true }

let set_flags_live (x : t) : t =
  { x with flags_live = Some true }

let add_problematic_operands (x : t) (prob_op : int list) : t =
  match x.problematic_operands with
  | Some prev ->
     { x with problematic_operands = Some (List.append prob_op prev) }
  | None ->
     { x with problematic_operands = Some prob_op }

let to_string (x : t) : string =
  let { tid;
        problematic_operands;
        left_val;
        right_val;
        flags_live;
        reason;
        desc } = x
  in
  let tid_str = Tid.to_string tid in
  let po_str = match problematic_operands with
    | Some ops -> String.concat ~sep:":" @@
                    List.map ops ~f:Int.to_string
    | None -> ""
  in
  let left_str = Option.value left_val ~default:"" in
  let right_str = Option.value right_val ~default:"" in
  let flags_live_str = match flags_live with
    | Some fl -> Bool.to_string fl
    | None -> ""
  in
  let reason_str = string_of_reason reason in
  sprintf
    "%s, %s, %s, %s, %s, %s, %s"
    tid_str
    po_str
    left_str
    right_str
    flags_live_str
    reason_str
    desc

let print_alert (alert : t) : unit =
    printf "Alert: %s\n" @@ to_string alert

let print_alerts : Set.t -> unit =
  Set.iter ~f:print_alert
