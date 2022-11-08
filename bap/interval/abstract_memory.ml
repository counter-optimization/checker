open Core
open Bap.Std

open Common

(* This is an implementation based on the paper 'Field-Sensitive Value *)
(* Analysis of Embedded C Programs with Union Types and Pointer *)
(* Arithmetics'. *)
(* currently assumes: *)
(* - little endian architecture *)

(* Problem: *)
(* - need a better way to pull specific information out of the product domains *)
(* - how do you pull the offset information out of the NumericDomain *)          
module Make(N : NumericDomain) = struct
  type region = Global | Heap | Stack [@@deriving sexp, bin_io]
  type base = region [@@deriving sexp, bin_io]

  type ptr = { region: region; offs: N.t; width: Size.t }

  (* the new environment operates over ValDom.t and pointers (PtrT of ptr) *)
  type scalar = PtrT of ptr | IntT of N.t

  (* it's important that a cell knows whether it holds a pointer or a scalar. *)
  (* for our model of memory, pointers are (region * offset) pairs, so taking *)
  (* e.g., the low 32 bits of a pointer doesn't type check *)
  type cell = { region: region; offs: N.t; valtype: scalar; width: Size.t }

  
end


