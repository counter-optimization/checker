open Core_kernel
open Bap_main
open Bap.Std
open Bap_primus.Std
open Graphlib.Std
open Monads.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

module SS = struct
  module T = struct
    include Set.Make_binable_using_comparator(String)
  end
  include T
  include Comparator.Make(T)
end

let package = "uarch-checker"

let output_csv_file_param = Extension.Configuration.parameter
                              ~doc:"CSV file where checker results will be stored"
                              Extension.Type.path
                              "output-csv-file"

let secrets_csv_file_param = Extension.Configuration.parameter
                               ~doc:"CSV file where secret function argument indices are stored"
                               Extension.Type.path
                               "secrets-csv-file"

let symex_profiling_output_file_path_param = Extension.Configuration.parameter
                                       ~doc:"CSV file where symex profiling data is stored"
                               Extension.Type.path
                               "symex-profiling-output-file"



let config_file_path_param = Extension.Configuration.parameter
                               ~doc:"Path to cio config file"
                               Extension.Type.string
                               "config-file"

let do_ss_checks_param = Extension.Configuration.flag
                           ~doc:"Do silent store checks?"
                           ~aliases:["silent-stores"; "x86-ss"]
                           "ss"

let do_cs_checks_param = Extension.Configuration.flag
                           ~doc:"Do comp simp checks?"
                           ~aliases:["comp-simp"; "x86-cs"]
                           "cs"

let no_symex_param = Extension.Configuration.flag
                       ~doc:"Don't do last ditch symex checks in addition to interval analysis"
                       "no-symex"

let int_of_sz = function
  | `r8 -> 8
  | `r16 -> 16
  | `r32 -> 32
  | `r64 -> 64
  | `r128 -> 128
  | `r256 -> 256

module AMD64SystemVABI = struct
  let flag_names : SS.t = SS.of_list ["CF"; "PF"; "AF"; "ZF"; "SF";
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
  
  let var_name_is_flag : string -> bool = SS.mem flag_names

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
    else if SS.mem flag_names name
    then Some 1
    else None
end

module ABI = AMD64SystemVABI

let string_flat_dom = KB.Domain.flat
                        ~empty:""
                        ~equal:String.equal
                        "string-flat-dom"

let string_powset_dom = KB.Domain.powerset
                          (module String)
                          ~inspect:String.sexp_of_t
                          "string-powerset-domain"

let string_opt_domain = KB.Domain.optional ~equal:String.equal "string_opt_domain"

let word_opt_domain = KB.Domain.optional ~equal:Word.equal "word_opt_domain"

let funcname_dom = KB.Domain.flat
                     ~empty:""
                     ~equal:String.equal
                     "funcname_dom"

let int_total_order_dom = KB.Domain.total
                 ~empty:(-1)
                 ~order:Int.compare
                 "time-ns-int-domain"

let int_opt_dom = KB.Domain.optional
                    ~equal:Int.equal
                    "int_opt_domain"

let tid_opt_domain = KB.Domain.optional
                       ~equal:Tid.equal
                       "tid_opt_domain"

let bool_opt_domain = KB.Domain.optional
                        ~equal:Bool.equal
                        "bool_opt_domain"

let int_list_opt_domain = KB.Domain.optional
                            ~equal:(List.equal Int.equal)
                            "int_list_opt_domain"

module EvalStats = struct
    type t = {
        total_considered : int;
        taint_pruned : int;
        interval_pruned : int;
        interproc_pruned : int;
        symex_pruned : int;
        interval_verified : int;
        symex_verified : int;
        unsupported_pruned : int;
      }

    let init = {
        total_considered = 0;
        taint_pruned = 0;
        interval_pruned = 0;
        interproc_pruned = 0;
        symex_pruned = 0;
        interval_verified = 0;
        unsupported_pruned = 0;
        symex_verified = 0;
      }

    let to_json_string s =
      let field_to_string value name = sprintf "\"%s\" : \"%d\"" name value in
      let total_considered = field_to_string s.total_considered "total_considered" in
      let taint_pruned = field_to_string s.taint_pruned "taint_pruned" in
      let interval_pruned = field_to_string s.interval_pruned "interval_pruned" in
      let interproc_pruned = field_to_string s.interproc_pruned "interproc_pruned" in
      let symex_pruned = field_to_string s.symex_pruned "symex_pruned" in
      let interval_verified = field_to_string s.interval_verified "interval_verified" in
      let symex_verified = field_to_string s.symex_verified "symex_verified" in
      let unsupported_pruned = field_to_string s.unsupported_pruned "unsupported_pruned" in
      let all_fields = [total_considered;
                        taint_pruned;
                        interval_pruned;
                        interproc_pruned;
                        symex_pruned;
                        unsupported_pruned;
                        interval_verified;
                        symex_verified]
      in
      let all_fields_newlined = String.concat ~sep:"\n" all_fields in
      let json = sprintf "{\n%s}" all_fields_newlined in
      json

    let combine x y =
      { total_considered = x.total_considered + y.total_considered;
        taint_pruned = x.taint_pruned + y.taint_pruned;
        interval_pruned = x.interval_pruned + y.interval_pruned;
        interproc_pruned = x.interproc_pruned + y.interproc_pruned;
        symex_pruned = x.symex_pruned + y.symex_pruned;
        interval_verified = x.interval_verified + y.interval_verified;
        unsupported_pruned = x.unsupported_pruned + y.unsupported_pruned;
        symex_verified = x.symex_verified + y.symex_verified }

    let incr_total_considered st =
      { st with
        total_considered = st.total_considered + 1 }

    let incr_taint_pruned st =
      { st with
        taint_pruned = st.taint_pruned + 1 }

    let incr_interval_pruned st =
      { st with
        interval_pruned = st.interval_pruned + 1 }

    let incr_interproc_pruned st =
      { st with
        interproc_pruned = st.interproc_pruned + 1 }

    let incr_symex_pruned st =
      { st with
        symex_pruned = st.symex_pruned + 1 }

    let incr_unsupported_pruned st =
      { st with
        unsupported_pruned = st.unsupported_pruned + 1 }

    let incr_symex_verified st =
      { st with
        symex_verified = st.symex_verified + 1 }

    let incr_interval_verified st =
      { st with
        interval_verified = st.interval_verified + 1 }
end

type 'a checker_res = {
    warns : 'a;
    cs_stats : EvalStats.t;
    ss_stats : EvalStats.t
  }

let combine_checker_res x y f =
  { warns = f x.warns y.warns;
    cs_stats = EvalStats.combine x.cs_stats y.cs_stats;
    ss_stats = EvalStats.combine x.ss_stats y.ss_stats;
  }

module CalleeRel = struct
  module T = struct
    type t = { caller : Tid.t; callee : Tid.t; callsite : Tid.t }
                 [@@deriving compare, bin_io, sexp]

    let print (r : t) : unit =
      Format.printf "CalleeRel: (caller: %a, callsite: %a, callee %a)\n%!"
        Tid.pp r.caller
        Tid.pp r.callsite
        Tid.pp r.callee

    let print_hum (r : t) ~(f : Tid.t -> string) : unit =
      Format.printf "CalleeRel: (caller: %s, callsite: %s, callee %s)\n%!"
        (f r.caller)
        (f r.callsite)
        (f r.callee)
  end

  module Cmp = struct
    include T
    include Comparator.Make(T)
  end

  include Cmp

  module Set = struct
    include Set.Make_binable_using_comparator(Cmp)

    let print (s : t) : unit =
      Format.printf "CalleeRel.Set:\n%!";
      iter s ~f:T.print

    let print_hum (s : t) ~(f : Tid.t -> string) : unit =
      Format.printf "CalleeRel.Set:\n%!";
      iter s ~f:(fun r -> T.print_hum r ~f)
  end
end

module ReturnInsnsGetter = struct
  type all_rets = (tid, Tid.comparator_witness) Set.t
  
  type t = all_rets
  
  type kbt

  open KB.Monad_infix

  let domain = KB.Domain.powerset (module Tid) ~inspect:Tid.sexp_of_t "tid-powerset-domain"

  let cls : (kbt, unit) KB.cls = KB.Class.declare
                                   ~public:true
                                   ~package
                                   "all-return-insn-tids" ()

  let all_rets : (kbt, t) KB.slot = KB.Class.property
                                      ~public:true
                                      ~package
                                      cls
                                      "all-rets-set"
                                      domain

  let is_return_label label : bool KB.t=
    KB.collect T.Semantics.slot label >>= fun (insn : Insn.t) ->
    let is_ret = Insn.is Insn.return insn in
    KB.return is_ret
  
  let empty : t = Set.empty (module Tid)

  let singleton : tid -> t = Set.singleton (module Tid)

  let label_to_tids label =
    KB.collect T.Label.addr label >>= fun maybe_addr ->
    KB.collect T.Label.name label >>= fun maybe_name ->
    KB.collect T.Semantics.slot label >>= fun (insn : Insn.t) ->
    let bir_terms = KB.Value.get Term.slot insn in
    (* bir_terms is a list of blk terms *)
    let tids_lists = List.map bir_terms ~f:(fun b ->
                                Term.enum jmp_t b
                                |> Sequence.map ~f:Term.tid
                                |> Sequence.to_list) in
    let tids = List.join tids_lists in
    KB.return @@ Set.of_list (module Tid) tids

  let compute_all_returns () : unit =
    Toplevel.exec
    (KB.objects T.Program.cls >>= fun labels ->
    let init_rets = KB.return empty in
    Seq.fold labels ~init:init_rets ~f:(fun all_rets label ->
        is_return_label label >>= fun is_ret ->
        if not is_ret
        then
          all_rets
        else
          label_to_tids label >>= fun ret_tids ->
          all_rets >>= fun all_rets ->
          KB.return @@ Set.union all_rets ret_tids)
    >>= fun all_ret_tids ->
    KB.Object.create cls >>= fun to_store ->
    KB.provide all_rets to_store all_ret_tids)(*  >>= fun () -> *)
  (* KB.return to_store *)

  let get_all_returns () : t =
    Toplevel.eval all_rets
      (KB.objects cls >>= fun objs ->
       KB.return @@ Seq.hd_exn objs)
    
  let build () : t = Set.empty (module Tid)

  let is_return : t -> tid -> bool = Set.mem
end

let sub_of_tid_for_prog (p : Program.t) (t : Tid.t) : sub term Or_error.t =
  match Term.find sub_t p t with
  | Some callee_sub -> Ok callee_sub
  | None ->
     Or_error.error_string @@
       Format.sprintf "Couldn't find callee sub for tid %a" Tid.pps t

module AnalysisBlackList = struct
  let blacklisted_func_names : string list = ["interrupt"; "plt"; "sodium_init"; "get_cpu_features"]

  let contains_blacklisted_func_name (subname : string) : bool =
    List.fold blacklisted_func_names ~init:false ~f:(fun any_bld blname ->
                any_bld || (String.is_substring subname ~substring:blname))

  let sub_is_blacklisted (sub : sub term) : bool =
    let name = Sub.name sub in
    contains_blacklisted_func_name name

  let sub_is_not_linked (sub : sub term) : bool =
    let blks = Term.enum blk_t sub in
    Seq.is_empty blks
end

(** Regions and basis maps *)
module Region = struct
  module T = struct
    type t = Global | Heap | Stack [@@deriving sexp, bin_io, compare]

    let equal r1 r2 : bool =
      match r1, r2 with
      | Global, Global
      | Heap, Heap
      | Stack, Stack -> true
      | _ -> false

    let to_string = function
      | Global -> "global"
      | Heap -> "heap"
      | Stack -> "stack"

    let pp x =
      Format.printf "%s%!" @@ to_string x
  end

  module Cmp = struct
    include T
    include Comparator.Make(T)
  end

  include Cmp

  module Set = struct
    include Set.Make_binable_using_comparator(Cmp)
    let from_region (r : T.t) = singleton r
  end
end

module CellType = struct
  type t = Scalar
         | Ptr
         | Unknown
         | Undef [@@deriving sexp, bin_io, compare, equal]

  let join t1 t2 =
    Ok (match t1, t2 with
        | Scalar, Scalar -> Scalar
        | Ptr, Ptr -> Ptr
        | Unknown, _ -> Unknown
        | _, Unknown -> Unknown
        | Undef, _ -> t2
        | _, Undef -> t1
        | _, _ -> Unknown)

  let empty = Unknown

  let order t1 t2 =
    let open KB.Order in
    match t1, t2 with
    | Unknown, Unknown -> EQ
    | Scalar, Scalar -> EQ
    | Ptr, Ptr -> EQ
    | Unknown, _ -> GT
    | _, Unknown -> LT
    | Undef, _ -> LT
    | _, Undef -> GT
    | _, _ -> NC

  let is_scalar : t -> bool = function
    | Scalar -> true
    | _ -> false

  let is_ptr : t -> bool = function
    | Ptr -> true
    | _ -> false

  let is_unknown : t -> bool = function
    | Unknown -> true
    | _ -> false

  let is_undef : t -> bool = function
    | Undef -> true
    | _ -> false

  let domain_name = "CellType"
  let domain = KB.Domain.define domain_name
                 ~inspect:sexp_of_t
                 ~join
                 ~empty
                 ~order
end

type cell_t = CellType.t

let binop_to_string ?(smtlib2compat : bool = false)
      (b : Bil.binop) : string =
  match b with
   | Bil.PLUS -> "+"
   | Bil.MINUS -> "-"
   | Bil.TIMES -> "*"
   | Bil.DIVIDE -> "/"
   | Bil.SDIVIDE -> "//"
   | Bil.MOD -> "%"
   | Bil.SMOD -> "%%"
   | Bil.LSHIFT -> "<<"
   | Bil.RSHIFT -> ">>"
   | Bil.ARSHIFT -> ">>>"
   | Bil.AND -> "&"
   | Bil.OR ->
      (* pipe is a problem for smtlib2 format string *)
      if smtlib2compat
      then "!$"
      else "|"
   | Bil.XOR -> "^"
   | Bil.EQ -> "="
   | Bil.NEQ -> "<>"
   | Bil.LT -> "<"
   | Bil.LE -> "<="
   | Bil.SLT -> "-<"
   | Bil.SLE -> "-<="

let unop_to_str (unop : Bil.unop) : string =
  (match unop with
   | Bil.NEG -> "~"
   | Bil.NOT -> "!")

let cast_to_str (cast : Bil.cast) : string =
  (match cast with
   | Bil.UNSIGNED -> "unsigned"
   | Bil.SIGNED -> "signed"
   | Bil.HIGH -> "high"
   | Bil.LOW -> "low")

(* this must output to a valid SMT-LIB2 symbol
   as it is used for syntactic tracking of memory
   cells in the symbolic compiler.
   according to this reference:
   https://smtlib.github.io/jSMTLIB/SMTLIBTutorial.pdf
   this is a string of the family:
   [a-zA-Z~!@$%^&*_+=<>.?/-] [0-9a-zA-Z~!@$%^&*_+=<>.?/-]* *)
let rec exp_to_string (e : Bil.exp) : string =
  let sz_to_str = function
      | `r8 -> "8"
      | `r16 -> "16"
      | `r32 -> "32"
      | `r64 -> "64"
      | `r128 -> "128"
      | `r256 -> "256" in
  let endian_to_str = function
    | BigEndian -> "be"
    | LittleEndian -> "le" in
  (match e with
   | Bil.Load (_, idx, endian, sz) ->
      let sz_str = sz_to_str sz in
      let end_str = endian_to_str endian in
      let idx_str = exp_to_string idx in
      sprintf "mem<%s?%s>?%s" idx_str end_str sz_str
   | Bil.Store (_, idx, value, endian , sz) -> 
      let sz_str = sz_to_str sz in
      let end_str = endian_to_str endian in
      let idx_str = exp_to_string idx in
      let val_str = exp_to_string value in
      sprintf "mem<%s?%s>?%s<-%s" idx_str end_str sz_str val_str
   | Bil.BinOp (op, left, right) ->
      exp_to_string left ^
        binop_to_string ~smtlib2compat:true op ^
          exp_to_string right
   | Bil.UnOp (op, left) ->
      unop_to_str op ^ exp_to_string left
   | Bil.Var v -> Var.name v
   | Bil.Int w ->
      (* just to_string'ing a Word.t adds the signedness and
         bitwidth to the string. used the formattters just gives
         the value as a string *)
      (* let () = printf "word is %a\n%!" Word.ppo w in *)
      Format.sprintf "%a" Word.pps w
      (* let () = printf "word pp to string is: %s\n%!" wpps in *)
      (* let ws = Word.to_string w in *)
      (* let () = printf "word as string is %s\n%!" ws in *)
      (* let ws = String.drop_prefix ws 1 in *)
      (* let ws = String.map ws *)
      (*            ~f:(fun c -> if Char.equal c ':' then '?' else c) in *)
      (* let () = printf "word to string is %s\n%!" ws in *)
      (* ws *)
   | Bil.Cast (cast, sz, exp) ->
      sprintf "%s(%d.%s)" (cast_to_str cast) sz (exp_to_string exp)
   | Bil.Let (var, exp, bod) ->
      sprintf "let%s=%sin%s" (Var.name var) (exp_to_string exp) (exp_to_string bod)
   | Bil.Unknown (str, _typ) -> sprintf "unknown%s" str
   | Bil.Ite (if_, then_, else_) ->
      sprintf "if%sthen%selse%s"
        (exp_to_string if_) (exp_to_string then_) (exp_to_string else_)
   | Bil.Extract (hi, lo, exp) ->
      sprintf "extract<%d.%d.%s>" hi lo (exp_to_string exp)
   | Bil.Concat (left, right) ->
      sprintf "<%s@@%s>" (exp_to_string left) (exp_to_string right))

let elt_to_tid (e : Blk.elt) : tid =
  match e with
  | `Jmp j -> Term.tid j
  | `Def d -> Term.tid d
  | `Phi p -> Term.tid p

(* technically, this way is not fully correct.
   calling jmp_is_return on a return will always
   return true, but if it returns true, it is
   not guaranteed to be a return (in the case
   of non-returning func calls. this is the
   general case, but the cryptographic code we
   analyze should not have these cases. the proper
   way would be to consult the semantics property
   "core:semantics" > "bap:insn-properties" > ":return"
   but this comes from the KB
 *)
let jmp_is_return (j : jmp term) : bool =
  (* this looks weird ("is return if Call.return is none"),
     but an x86_64 return insn is lifted to an indirect
     call to the saved return pointer with no return. *)
  match Jmp.kind j with
  | Call c -> Option.is_none @@ Call.return c
  | _ -> false

let ai_widen_threshold = 4

module type CheckerInterp = sig
  type t
  val denote_exp : tid -> Bil.exp -> t list
end
