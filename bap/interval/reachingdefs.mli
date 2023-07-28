open Core_kernel
open Bap.Std
open Graphlib.Std

type varname = string

type defset

type sol

type t

val run_on_cfg : 'g. (module Graph with type t = 'g and type node = Calling_context.t) -> 'g -> sub term -> Blk.elt Tid_map.t -> Flag_ownership.t -> Calling_context.t -> t
