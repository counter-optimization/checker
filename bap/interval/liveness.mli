open Core
open Bap.Std
open Graphlib.Std
open Common

type env = SS.t

type t

val run_on_cfg : (module Graph with type t = 'a and type node = Calling_context.t) -> 'a -> Edge_builder.tidmap -> t

val live_at_tid : tid -> t -> env
