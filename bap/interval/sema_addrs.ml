open Core_kernel
open Bap.Std
       
module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

let package = Common.package

open KB.Syntax

let addr_domain = KB.Domain.flat
                    ~empty:Bitvec.zero
                    ~inspect:(fun bv -> Sexp.Atom (Bitvec.to_string bv))
                    ~equal:Bitvec.equal
                    "addr-flat-domain"

let slot = KB.Class.property
             ~package
             ~public:true
             ~desc:"Semantics addr slot for looking labels up"
             T.Semantics.cls
             "sema-addr"
             addr_domain

let provide_label () =
  KB.Rule.(declare ~package "sema-label-map" |>
           require T.Label.addr |>
           require T.Semantics.slot |>
           provide T.Semantics.slot |>
           comment "provides a map up the object/val back to tid");
  KB.observe T.Label.addr @@ fun label -> function
  | Some addr ->
    let* sema = KB.collect T.Semantics.slot label in
    let sema' = KB.Value.put slot sema addr in
    KB.provide T.Semantics.slot label sema'
  | None -> KB.return ()

let init () =
  provide_label ()
  
