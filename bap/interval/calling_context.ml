open Core
open Common
open Bap.Std
open Regular.Std
open Graphlib.Std
open Monads.Std

module KB = Bap_knowledge.Knowledge

module T = struct
  type t = Tid.t list [@@deriving compare, bin_io, sexp, hash]

  let module_name = Some "Calling_context"

  let version = "0.0.0"

  let to_string (cc : t) : string =
    let rec loop cc =
      match cc with
      | [] -> ""
      | x :: xs -> sprintf "%a" Tid.pps x
    in
    sprintf "tid%s" @@ loop cc

  let pp ppf (cc : t) =
    let rec loop ppf cc =
      match cc with
      | [] -> ()
      | x :: xs ->
         let tid_string = Tid.to_string x in
         Format.fprintf ppf " %s " tid_string
    in
    Format.fprintf ppf "[";
    loop ppf cc;
    Format.fprintf ppf "]"
end

module Regular = struct
  include Regular.Make(T)
end

module Edge = struct
  type 'a t = T.t * T.t * 'a

  let from_ (e : 'a t) : T.t =
    let (from', to', _) = e in
    from'

  let to_ (e : 'a t) : T.t =
    let (from', to', _) = e in
    to'
end

type 'a edges = 'a Edge.t list

include T
include Regular

let to_insn_tid (ctxt : t) : Tid.t =
  match ctxt with
  | [] ->
     failwith "in Calling_context.to_insn_tid, couldn't get last insn tid of empty calling context"
  | n :: ns -> n

let add_callsite (cs : Tid.t) (ctxt : t) : t = List.cons cs ctxt

let add_node (n : Tid.t) (ctxt : t) : t = List.cons n ctxt

let of_tid (t : Tid.t) : t = [t]

let of_edge (e : Edge_builder.edge) : bool Edge.t =
  let (from', to', is_interproc) = e in
  (of_tid from', of_tid to', is_interproc)
