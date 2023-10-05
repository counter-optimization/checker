open Core_kernel

let package = "uarch-checker"

let bap_cl_arg_enum = [("app", Logs.App);
                       ("err", Logs.Error);
                       ("error", Logs.Error);
                       ("warn", Logs.Warning);
                       ("warning", Logs.Warning);
                       ("info", Logs.Info);
                       ("debug", Logs.Debug);
                       ("dbg", Logs.Debug)]

let default_level = Logs.Info
let global_level : Logs.level option ref = ref None

let create_src ?doc name =
  let doc = match doc with
    | Some doc -> doc
    | None -> ""
  in
  Logs.Src.create ~doc @@ sprintf "%s.%s" package name

let create_header src = Logs.Src.name src

let init () =
  Logs.set_reporter (Logs.format_reporter ())

let set_global_level lvl =
  global_level := Some lvl;
  Logs.set_level ~all:true (Some lvl)

(* module type ModuleSpecific = sig *)
(*   val src : Logs.src *)
(*   val header : string *)
(* end *)

(* module Logger(MS : ModuleSpecific) : sig *)
(*   include ModuleSpecific *)
(*   val info : ('a, Stdlib.Format.formatter, unit, 'b) Stdlib.format4 -> 'a *)
(* end = struct *)
(*   include MS *)
(*   let info msg = Logs.info ~src (fun m -> m ~header msg) *)
(* end *)
