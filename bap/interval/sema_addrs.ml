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
  KB.promise T.Semantics.slot @@ fun label ->
  let* addr = label-->T.Label.addr in
  let addr = match addr with
  | Some addr -> addr
  | None -> Bitvec.zero
  in
  let* sema = label-->T.Semantics.slot in
  KB.return @@ KB.Value.put slot sema addr

let debug_print () =
  KB.observe slot @@ fun sema addr ->
  let* asm = sema-->Insn.Slot.asm in
  let addr = Bitvec.to_string addr in
  KB.return @@ printf "[Sema_addrs] %s, %s\n%!" addr asm

let init () =
  provide_label ();
  debug_print ()
  
