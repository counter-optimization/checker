open Core_kernel
open Bap_main
open Bap.Std
open Graphlib.Std
open Monads.Std
module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB
module SS = String.Set

let package = "uarch-checker"

module L = struct
  include Dolog.Log
  let log_prefix = sprintf "%s.common" package
  let () = set_prefix log_prefix
end

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

let debug_tids_param = Extension.Configuration.parameter
                         ~doc:"Path to targeted tids file"
                         (Extension.Type.some Extension.Type.non_dir_file)
                         "debug-tids"

let dbg_print_envs = Extension.Configuration.flag
                       ~doc:"Print input envs to each elt in main interpreter"
                       ~aliases:["print-envs"]
                       "dbg-print-envs"

let global_log_level_param = Extension.Configuration.parameter
                               ~doc:"Global log level"
                               ~aliases:["log"]
                               (Extension.Type.enum
                                  Uc_log.bap_cl_arg_enum)
                               "log-level"

let do_ss_checks_param = Extension.Configuration.flag
                           ~doc:"Do silent store checks?"
                           ~aliases:["silent-stores"; "x86-ss"]
                           "ss"

let do_cs_checks_param = Extension.Configuration.flag
                           ~doc:"Do comp simp checks?"
                           ~aliases:["comp-simp"; "x86-cs"]
                           "cs"

let do_dmp_checks_param = Extension.Configuration.flag
                            ~doc:"Do DMP checks?"
                            "dmp"

let no_symex_param = Extension.Configuration.flag
                       ~doc:"Don't do last ditch symex checks in addition to interval analysis"
                       "no-symex"

let debug_dump = Extension.Configuration.flag
                   ~doc:"Also print BIR for each sub and global initial Knowledge_base"
                   "dump-bir-kb"

let is_double_check = Extension.Configuration.flag
                        ~doc:"Is this a double-checking run?"
                        "double-check"

let cache_tainted_args = Extension.Configuration.parameter
                           ~doc:"File to cache tainted args from interpoc taint propagation in"
                           ~aliases:["taint"]
                           (Extension.Type.some Extension.Type.path)
                           "taint-cache"

let eval_prune_first = Extension.Configuration.parameter
                         ~doc:"Eval for ASPLOS submission only: don't consider terms if virt addr is in this file"
                         (Extension.Type.some Extension.Type.non_dir_file)
                         "eval-elim-vaddrs"

let skip_check (elt : Blk.elt) (skip_addrs : Int.Set.t ref) : bool =
  let addr (type a) (term : a Term.t) : int =
    match Term.get_attr term Disasm.insn with
    | Some sema ->
      KB.Value.get Sema_addrs.slot sema
      |> Bitvec.to_int
    | None ->
      L.warn "skip_check: no addr for %a" Tid.ppo @@ Term.tid term;
      0
  in
  if Int.Set.is_empty !skip_addrs
  then false
  else
    let addr = match elt with
      | `Def d -> addr d
      | `Jmp j -> addr j
      | `Phi p -> addr p
    in
    Int.Set.mem !skip_addrs addr

let get_taint_cache
      (projctxt : Bap_main.ctxt) : string option =
  Extension.Configuration.get projctxt cache_tainted_args

let do_memtrace =
  Extension.Configuration.flag
    ~doc:"Enable memtrace for a memtrace run"
    "memtrace"

let elt_to_tid (e : Blk.elt) : tid =
  match e with
  | `Jmp j -> Term.tid j
  | `Def d -> Term.tid d
  | `Phi p -> Term.tid p

let map_diff (type a b) (m1 : (a, b, _) Map.t) (m2 : (a, b, _) Map.t)
      ~equal : a list =
  let get_same_and_differing_keys m1 m2 =
    let keys1 = Map.key_set m1 in
    let keys2 = Map.key_set m2 in
    let shared = Set.inter keys1 keys2 in
    let only1 = Set.diff keys1 shared in
    let only2 = Set.diff keys2 shared in
    let differing = Set.union only1 only2 in
    shared, differing in
  let data_same key m1 m2 =
    equal (Map.find_exn m1 key) (Map.find_exn m2 key) in
  let same, differ = get_same_and_differing_keys m1 m2 in
  let same_differ = Set.fold same ~init:[]
                      ~f:(fun total same_key ->
                        if data_same same_key m1 m2
                        then total
                        else same_key :: total) in
  List.append (Set.to_list differ) same_differ

let int_of_sz = function
  | `r8 -> 8
  | `r16 -> 16
  | `r32 -> 32
  | `r64 -> 64
  | `r128 -> 128
  | `r256 -> 256

module AMD64SystemVABI = Abi.AMD64SystemVABI
module ABI = AMD64SystemVABI

let term_opt_domain : def term option KB.domain = KB.Domain.optional ~equal:Term.same "term-opt-domain"

let int_flat_dom = KB.Domain.flat
                     ~inspect:Int.sexp_of_t
                     ~empty:(-1)
                     ~equal:Int.equal
                     "int-flat-dom"

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

let tids_of_blk (blk : blk term) : Tid.Set.t =
  Blk.elts blk
  |> Seq.fold ~init:Tid.Set.empty ~f:(fun tids elt ->
    Tid.Set.add tids @@ elt_to_tid elt)

let tids_of_blks (blks : blk term list) : Tid.Set.t =
  List.fold blks ~init:Tid.Set.empty ~f:(fun tids blk ->
    Tid.Set.union (tids_of_blk blk) tids)

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

let sub_of_tid_for_prog (p : Program.t) (t : Tid.t) : sub term Or_error.t =
  match Term.find sub_t p t with
  | Some callee_sub -> Ok callee_sub
  | None ->
    Or_error.error_string @@
    Format.sprintf "Couldn't find callee sub for tid %a" Tid.pps t

module AnalysisBlackList : sig
  val sub_is_blacklisted : sub term -> bool
  val sub_is_not_linked : sub term -> bool
end = struct
  let blacklisted_func_names = ["sodium_init";
                                "get_cpu_features"]
                               @ Dmp_helpers.checker_blacklisted_fns
                               |> String.Set.of_list

  let has_blacklisted_substring name =
    let targets = ["plt"; "interrupt"] in
    List.exists targets ~f:(fun substring ->
      String.Caseless.is_substring name ~substring)

  let is_blacklisted name =
    String.Set.mem blacklisted_func_names name ||
    Dmp_helpers.is_blacklisted name ||
    has_blacklisted_substring name

  let sub_is_blacklisted sub =
    Sub.name sub |> is_blacklisted 

  let sub_is_not_linked sub =
    Term.enum blk_t sub |> Seq.is_empty 
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

let ai_widen_threshold = 3

module type CheckerInterp = sig
  type t
  val denote_exp : tid -> Bil.exp -> t list
end

let sub_of_tid (proj : Project.t) (t : Tid.t)
  : sub term option =
  let prog = Project.program proj in
  Term.find sub_t prog t
