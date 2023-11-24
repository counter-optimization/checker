open Core_kernel
open Bap.Std
open Monads.Std

module BapG = Graphlib.Std
module OcamlG = Graph

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

let log_prefix = sprintf "%s.uc_fixpoint" Common.package
module L = struct
  include Dolog.Log
  let () = set_prefix log_prefix
end

module Chaotic(Data : OcamlG.ChaoticIteration.Data with type edge = Uc_graph_builder.UcOcamlG.T.E.t) = struct
  module G = Uc_graph_builder.UcOcamlG.T
  module WTO = OcamlG.WeakTopological.Make(G)
  module Chao = OcamlG.ChaoticIteration.Make(G)(Data)

  let get_wto g start_node = WTO.recursive_scc g start_node

  let compute g wto f_init wideset widethresh =
    Chao.recurse g wto f_init wideset widethresh
end
