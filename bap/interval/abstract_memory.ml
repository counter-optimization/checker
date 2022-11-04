open Core_kernel
open Bap.Std

open Common

(* This is an implementation based on the paper 'Field-Sensitive Value *)
(* Analysis of Embedded C Programs with Union Types and Pointer *)
(* Arithmetics' that was written for use in the Astrée static *)
(* analyzer. *)

(* Defaults to 64 bit, little endian. TODO: parametrize and creatable *)
(* for different endiannesses *)
module Make(OffsDom : NumericDomain)(ValDom : NumericDomain) = struct
  type region = Global | Heap | Stack

  type celltype = ScalarCellType of int | PtrCellType
  type cell = { regions: region list; offs: OffsDom.t; ct: celltype }
  type ptr = Ptr { regions: region list; offs: OffsDom.t; pt: celltype }
           | Null | Invalid
end


