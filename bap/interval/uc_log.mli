open Core_kernel

module L = Dolog.Log

val init : L.log_level -> unit

val set_global_level : L.log_level -> unit

val bap_cl_arg_enum : (string * L.log_level) list
