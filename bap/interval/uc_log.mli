open Core_kernel

val init : unit -> unit

val set_global_level : Logs.level -> unit

val bap_cl_arg_enum : (string * Logs.level) list

val create_src : ?doc:string -> string -> Logs.src
