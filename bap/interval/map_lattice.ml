open Core_kernel
open Bap.Std

module KB = Bap_knowledge.Knowledge 

              (**
                 For now, just a map lattice from Tid -> Interval.
                  For future, make this a functor that takes a lattice and
                  creates the map.
               *)

module Interval = struct
  module K = String

  module M = Map.Make_binable_using_comparator(String)
  include M
end

