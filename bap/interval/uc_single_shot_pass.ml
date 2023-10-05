open Core_kernel
open Bap.Std
       
module KB = Bap_knowledge.Knowledge
module T = Bap_core_theory.Theory

module State = Monads.Std.Monad.State

module type PASS = sig
  type t
    
  val onjmp : jmp term -> t
  val ondef : def term -> t
  val onphi : phi term -> t
end

(* module IdxInsnCollector : PASS = struct *)
(*   module TS = Tid.Set *)
                
(*   type state = TS.t *)
(*   type t = (unit, state) State.t *)
             
(*   let is_r11 = String.Caseless.is_substring *)
(*                  ~substring:"%r11" *)

(*   let is_sbb = String.Caseless.is_substring *)
(*                    ~substring:"sbbq" *)

(*   let try_get_idx dt = *)
(*     let open Option.Monad_infix in *)
(*     Term.get_attr dt Disasm.insn >>= fun sema -> *)
(*     KB.Value.get T.Semantics.code sema >>= fun asm -> *)
(*     if is_sbb asm *)
(*     then *)
(*       let fields = String.split asm ~on:" " in *)
(*       match fields with *)
(*       | opcode :: idx :: reg :: [] when is_r11 reg -> *)
(*         String.drop_prefix *)
(*       | _ -> None *)
(*     else None *)

(*   let ondef dt = *)
(*     let sema = get_sema dt in *)
    
    
(*   let onjmp _ = State.return () *)
(*   let onphi _ = State.return () *)
(* end *)
