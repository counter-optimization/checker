open Core_kernel
open Bap.Std

module type Env = sig
  type t
  type v
  val lookup : string -> t -> v
  val set : string -> v -> t -> t
end

module type InverterS = sig
end

module Identity(V : Abstract.NumericDomain) : InverterS = struct
end

module Compute(V : Abstract.NumericDomain)
    (E : Env with type v := V.t) : sig
  val run : E.t -> Bil.exp -> E.t                              
end = struct
  type node = {
    up : V.t;
    down : V.t;
    exp : Bil.exp;
  }
  
  type t
  
  let run (env : E.t) (exp : Bil.exp) : E.t =
    env
end
