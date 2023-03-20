open Core
open Bap.Std
open Common

type t

type var = string

type typed_var

type typd

val run : def term list -> (var -> int option) -> t

val get_type : var -> t -> typd option

val get_bitwidth : var -> t -> int option

val get_all_typed_vars : t -> var list

val print : t -> unit

val narrow_to_vars : Common.SS.t -> t -> t
