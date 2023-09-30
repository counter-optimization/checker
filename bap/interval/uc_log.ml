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

let init () =
  Logs.set_reporter (Logs.format_reporter ())

let set_global_level lvl =
  global_level := Some lvl;
  Logs.set_level ~all:true (Some lvl)
