open Core_kernel
open Bap.Std

module Theory = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge
module Taint = Checker_taint.Analysis

open KB.Syntax
       
let package = Common.package

let log_prefix = sprintf "%s.uc_inargs" package
module L = struct
  include Dolog.Log
  let () = set_prefix log_prefix
end

type subname = string

module T = struct
  type t

  let cls : (t, unit) KB.Class.t =
    KB.Class.declare ~public:true
      ~package
      ~desc:"holds input state for analyzing func with subname"
      "in-state" ()

  let name_dom = KB.Domain.flat
                   ~empty:""
                   ~equal:String.equal
                   "name_dom"

  let name : (t, string) KB.slot =
    KB.Class.property ~package cls "in-state-sub-name" name_dom
end

include T

module TaintInState = struct
  type t = String.Set.t

  let log_prefix = sprintf "%s.taint" log_prefix
  module L = struct
    include Dolog.Log
    let () = set_prefix log_prefix
  end

  let taintdom =
    KB.Domain.powerset (module String.Set.Elt)
      ~inspect:String.sexp_of_t "taint-in-state-dom"

  let tainted_regs : (T.t, t) KB.slot =
    KB.Class.property ~package cls "taint-map" taintdom

  let get subname : String.Set.t =
    let args = ref String.Set.empty in
    Toplevel.exec begin
      KB.objects T.cls >>= fun instates ->
      KB.Seq.find instates ~f:(fun obj ->
        KB.collect T.name obj >>= fun name ->
        KB.return @@
        String.Caseless.equal subname name)
      >>= function
        | Some obj ->
          KB.collect tainted_regs obj >>= fun taints ->
          KB.return (args := taints)
        | None -> KB.return ()
    end;
    !args

  let config (module ABI : Abi.ABIDef) (c : Config.t) : unit =
    let build_sub_taint name taintedidxs =
      KB.Object.create T.cls >>= fun st ->
      KB.provide T.name st name >>= fun () ->
      let taintedargs = Int.Set.to_list taintedidxs
                        |> List.map ~f:ABI.arg_from_idx
                        |> List.filter ~f:Option.is_some
                        |> List.map ~f:(fun e -> Option.value_exn e)
                        |> String.Set.of_list in
      L.debug "Taint input state for %s: %s" name @@ List.to_string ~f:Fn.id @@ String.Set.to_list taintedargs;
      KB.provide tainted_regs st taintedargs
    in
    let config_secs = Config.get_secret_args c in
    String.Map.iteri config_secs ~f:(fun ~key ~data ->
      Toplevel.exec @@ build_sub_taint key data)
end
