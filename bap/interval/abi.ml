open Core_kernel
open Bap.Std

module type ABIDef = sig
  val stack_reg : string
  val frame_reg : string

  val flag_names : Set.M(String).t

  val gpr_arg_names : string list
  val gpr_names : string list

  val vector_arg_names : string list

  val vector_arg_width : int
  val gpr_arg_width : int
end

module AMD64SystemVABI = struct
  let stack_reg = "RSP"

  let frame_reg = "RBP"

  let flag_names = Set.of_list (module String) ["CF"; "PF"; "AF"; "ZF"; "SF";
                                                "TF"; "IF"; "DF"; "OF"]

  let gpr_arg_names = ["RDI"; "RSI"; "RDX"; "RCX"; "R8"; "R9"]

  let gpr_names = ["RAX"; "RBX"; "RCX"; "RDX"; "RDI"; "RSI";
                   "R8"; "R9"; "R10"; "R11"; "R12"; "R13";
                   "R14"; "R15"; "RSP"; "RBP"]

  let vectorreg_arg_names_aliased = ["XMM0"; "XMM1"; "XMM2"; "XMM3"; "XMM4";
                                     "XMM5"; "XMM6"; "XMM7"]

  (* i think bap uses YMMN instead of XMMN similar to how it uses RAX
     (like low:32[RAX]) instead of EAX *)
  let vectorreg_arg_names_unaliased = ["YMM0"; "YMM1"; "YMM2"; "YMM3"; "YMM4";
                                       "YMM5"; "YMM6"; "YMM7"]

  let vector_arg_names = List.append vectorreg_arg_names_unaliased vectorreg_arg_names_aliased

  let vector_arg_width = 256

  let arg_names = List.append gpr_arg_names vector_arg_names

  let gpr_arg_width = 64

  let var_name_is_flag : string -> bool = Set.mem flag_names

  let var_name_is_arg : string -> bool = List.mem arg_names ~equal:String.equal

  let var_name_is_vector_arg : string -> bool = List.mem vector_arg_names ~equal:String.equal

  let var_name_is_gpr : string -> bool = List.mem gpr_arg_names ~equal:String.equal

  let size_of_var_name name : int option =
    let equal = String.equal in
    if String.is_substring name ~substring:"YMM"
    then Some 256
    else if String.is_substring name ~substring:"XMM"
    then Some 128
    else if List.mem gpr_names name ~equal
    then Some 64
    else if Set.mem flag_names name
    then Some 1
    else None
end
