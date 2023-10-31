open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge
module Taint = Checker_taint.Analysis

let package = Common.package

let log_prefix = sprintf "%s.uc_inargs" package
module L = struct
  include Dolog.Log
  let () = set_prefix log_prefix
end

module Taint = struct
  type 'a map = (string, Taint.t, 'a) Map.t
               
  let taintdom : _ map KB.domain =
    KB.Domain.mapping Checker_taint.Analysis.Cmp.comparator
      ~inspect:Taint.sexp_of_t
      ~join:(fun x y -> Ok (Taint.join x y))
      ~equal:Taint.equal
      "taint-inargs-dom"

  let cls : (Taint.t, unit) KB.Class.t =
    KB.Class.declare
      ~public:true
      ~package
      ~desc:"holds taint summaries for each function"
      "taint-inargs-cls" ()

  let slot : (Taint.t, _ map) KB.slot =
    KB.Class.property ~package cls "taint-map" taintdom

  let extract (type a) (prod : a) : _ map =
    Taint.of_prod Taint.get prod
end
