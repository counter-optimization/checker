open Core_kernel

module L = Dolog.Log

let package = "uarch-checker"

let bap_cl_arg_enum = [("err", L.ERROR);
                       ("error", L.ERROR);
                       ("warn", L.WARN);
                       ("warning", L.WARN);
                       ("info", L.INFO);
                       ("debug", L.DEBUG);
                       ("dbg", L.DEBUG)]

let default_level = L.INFO
let global_level : L.log_level ref = ref default_level

let set_global_level lvl =
  global_level := lvl;
  L.set_log_level lvl

let init global_lvl =
  set_global_level global_lvl;
  L.color_on ();
  L.set_output stdout;
  L.info "%s" "logging initialized"
  
