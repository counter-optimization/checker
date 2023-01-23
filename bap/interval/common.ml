open Core
open Bap_main
open Bap.Std
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

let target_func_param = Extension.Configuration.parameter
                            ~doc:"Which top-level function to check"
                            Extension.Type.string
                            "target-function"

let output_csv_file_param = Extension.Configuration.parameter
                              ~doc:"CSV file where checker results will be stored"
                              Extension.Type.string
                              "output-csv-file"

let secrets_csv_file_param = Extension.Configuration.parameter
                               ~doc:"CSV file where secret function argument indices are stored"
                               Extension.Type.string
                              "secrets-csv-file"

module AMD64SystemVABI = struct
  let flag_names : SS.t = SS.of_list ["CF"; "PF"; "AF"; "ZF"; "SF";
                                      "TF"; "IF"; "DF"; "OF"]

  let gpr_arg_names = ["RDI"; "RSI"; "RDX"; "RCX"; "R8"; "R9"]

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
end

module ABI = AMD64SystemVABI

let string_powset_dom = KB.Domain.powerset
                          (module String)
                          ~inspect:String.sexp_of_t
                          "string-powerset-domain"

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
    
  let build () : t =
    let computation = KB.objects T.Program.cls >>= fun labels ->
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
                      KB.provide all_rets to_store all_ret_tids >>= fun () ->
                      KB.return to_store in
    let cur_st = Toplevel.current () in 
    match KB.run cls computation cur_st with
    | Ok (ret_tids, _st') -> KB.Value.get all_rets ret_tids
    | Error e ->
       failwith @@
         sprintf "in ReturnInsnsGetter.build, error : %s" (KB.Conflict.to_string e)

  let is_return (tid : tid) (all_rets : all_rets) : bool = Set.mem all_rets tid
end

let sub_of_tid_for_prog (p : Program.t) (t : Tid.t) : sub term Or_error.t =
  match Term.find sub_t p t with
  | Some callee_sub -> Ok callee_sub
  | None ->
     Or_error.error_string @@
       Format.sprintf "Couldn't find callee sub for tid %a" Tid.pps t

module AnalysisBlackList = struct
  let blacklisted_func_names : string list = ["interrupt"; "plt"]

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

type (_, _) eq =
  | Eq : ('a, 'a) eq
  | Neq

module type KeyT = sig
  type 'a k
  val create : string -> 'a k
  val eq_type : 'a k -> 'b k -> ('a, 'b) eq
  val name : 'a k -> string
end

(* This is the naive external domain key from *)
(* EVA in the Frama-C framework. *)
module DomainKey : KeyT = struct
  type _ key = ..

  module type K = sig
    type t
    type _ key += Key : t key
    val name : string
  end

  type 'a k = (module K with type t = 'a)

  let name (type a) (elt : a k) : string =
    let module A = (val elt : K with type t = a) in
    A.name

  let create (type a) (n : string) : a k =
    let module M = struct
        type t = a
        type _ key += Key : t key
        let name = n
      end in
    (module M : K with type t = a)

  let eq_type (type a) (type b) (x : a k) (y : b k) : (a, b) eq =
    let module A = (val x : K with type t = a) in
    let module B = (val y : K with type t = b) in
    match A.Key with
    | B.Key -> Eq
    | _ -> Neq
end

let binop_to_string (b : Bil.binop) : string =
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
   | Bil.OR -> "|"
   | Bil.XOR -> "^"
   | Bil.EQ -> "="
   | Bil.NEQ -> "<>"
   | Bil.LT -> "<"
   | Bil.LE -> "<="
   | Bil.SLT -> "-<"
   | Bil.SLE -> "-<="

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

let ai_widen_threshold = 10

module type NumericDomain = sig
  type t

  val key : t DomainKey.k
  val get : 'a DomainKey.k -> (t -> 'a) option
  val set : 'a DomainKey.k -> t -> 'a -> t

  val bot : t
  val top : t
  val make_top : int -> bool -> t 

  val b1 : t
  val b0 : t

  val order : t -> t -> KB.Order.partial
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
  val contains : t -> t -> bool (* Useful for checkers *)

  (* BIL specific *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val sdiv : t -> t -> t
  val umod : t -> t -> t
  val smod : t -> t -> t
  val lshift : t -> t -> t
  val rshift : t -> t -> t
  val arshift : t -> t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t

  val neg : t -> t
  val lnot : t -> t

  val extract : t -> int -> int -> t
  val concat : t -> t -> t
  
  val booleq : t -> t -> t
  val boolneq : t -> t -> t
  val boollt : t -> t -> t
  val boolle : t -> t -> t
  val boolslt : t -> t -> t
  val boolsle : t -> t -> t
  val could_be_true : t -> bool
  val could_be_false : t -> bool

  val unsigned : int -> t -> t
  val signed : int -> t -> t
  val low : int -> t -> t
  val high : int -> t -> t

  val to_string : t -> string
  val of_int : ?width:int -> int -> t
  val of_word : word -> t
  val of_z : ?width:int -> Z.t -> t
  val bitwidth : t -> int
  val sexp_of_t : t -> Sexp.t
end

(* module type NumericEnvT = *)
(*   functor (N : NumericDomain) -> *)
(*   sig *)
(*     type t *)

(*     val lookup : string -> t -> N.t *)
(*     val set : string -> N.t -> t -> t *)
(*     val mem : t -> string -> bool *)
(*     val equal : t -> t -> bool *)
(*     val empty : t *)
(*     val empty_for_entry : t *)
(*     val empty_with_args : t *)
(*     val merge : t -> t -> t *)
(*     val widen_threshold : int *)
(*     val widen_with_step : int -> 'a -> t -> t -> t *)
(*     val pp : t -> unit *)
(*   end *)

module type MemoryT =
  sig
    type t
    
    type v
    
    type regions
    
    type region
    
    type valtypes

    type 'a err = ('a, Error.t) Result.t

    val empty : t
    
    val lookup : string -> t -> v

    val init_arg : name:string -> t -> t
    
    val set : string -> v -> t -> t

    val unset : string -> t -> t
    
    val equal : t -> t -> bool
    
    val set_rsp : int -> t -> t err
    
    val set_rbp : int -> t -> t err
    
    val set_img : t -> Image.t -> t

    val set_stack_canary : t -> t
    
    val holds_ptr : string -> t -> bool
    
    val setptr : name:string -> regions:regions -> offs:v -> width:v -> t -> t
    
    val unptr : name:string -> t -> t
    
    (* val update_on_assn : lhs:Var.t -> rhs:v -> t -> t *)
    
    val load_of_bil_exp : Bil.exp -> v -> Size.t -> t -> (v * t) err
    
    val store_of_bil_exp : Bil.exp -> offs:v -> data:v -> size:Size.t -> t -> t err

    val store_global : addr:addr -> data:word -> valtype:CellType.t -> t -> t err

    val havoc_on_call : t -> t

    val merge : t -> t -> t
    
    val widen_threshold : int
    
    val widen_with_step : int -> 'a -> t -> t -> t
    
    val pp : t -> unit
  end

module NumericEnv(ValueDom : NumericDomain)
       : (MemoryT with type v := ValueDom.t
                   and type regions := unit
                   and type region := unit
                   and type valtypes := unit) = struct
  
  module M = Map.Make_binable_using_comparator(String)
  
  module G = Graphlib.Make(Tid)(Unit)
    
  type v = ValueDom.t
  
  type t = ValueDom.t M.t

  type 'a err = ('a, Error.t) Result.t

  let empty : t = M.empty
  
  let stack_ptr = "RSP"
  
  let frame_ptr = "RBP"
  
  let start_stack_addr = Word.of_int ~width:64 262144
  
  let x86_64_default_taint = ["RDI"; "RSI"; "RDX"; "RCX"; "R8"; "R9"]

  let holds_ptr var_name env = false

  let lookup name env =
    match M.find env name with
    | Some v -> v
    | None -> ValueDom.top

  let set name v env : t = M.set env ~key:name ~data:v

  let init_arg ~(name : string) env : t = M.set env ~key:name ~data:ValueDom.top

  let unset name env : t = M.remove env name 

  let set_rsp offs env = Ok (set "RSP" (ValueDom.of_int offs) env)
  
  let set_rbp offs env = Ok (set "RBP" (ValueDom.of_int offs) env)
  
  let set_img env img = env

  let setptr ~name ~regions ~offs ~width env = env

  let set_stack_canary env = env
  
  let unptr ~name env = env
  
  let update_on_assn ~lhs ~rhs env = env

  let load_of_bil_exp (e : Bil.exp) _offs _sz env = Ok (ValueDom.top, env)
  
  let store_of_bil_exp (e : Bil.exp) ~offs ~data ~size env = Ok env

  let store_global ~addr ~data ~valtype env = Ok env

  let havoc_on_call env = env
  
  let mem = M.mem
  
  let equal = M.equal ValueDom.equal

  let merge env1 env2 : t =
    let merge_helper ~key ~data prev =
      if M.mem prev key
      then
        begin
          let last = M.find_exn prev key in
          let merged = ValueDom.join last data in
          M.set prev ~key ~data:merged
        end
      else M.set prev ~key ~data in
    M.fold env2 ~init:env1 ~f:merge_helper

  let widen_threshold = ai_widen_threshold
  
  let widen_with_step steps n prev_state new_state : t =
    let get_differing_keys prev_state new_state =
      M.fold prev_state ~init:Seq.empty ~f:(fun ~key ~data acc ->
          let next = M.find_exn new_state key in
          (* let () = printf "in widen_with_step:get_differing_keys, prev is %s, next is %s\n" *)
          (*                 (ValueDom.to_string data) (ValueDom.to_string next) in *)
          if ValueDom.equal data next
          then
            (* let () = printf "the values were equal\n%!" in *)
            acc
          else
            (* let () = printf "the values were not equal\n%!" in *)
            Seq.cons key acc)
    in
    let widen_state prev_state new_state =
      let changed_keys = get_differing_keys prev_state new_state in
      (* let () = printf "in widen_with_step:widen_state, changed keys are:\n%!"; *)
               (* Seq.iter changed_keys ~f:(fun var -> printf "changed key: %s\n%!" var) in *)
      Seq.fold changed_keys ~init:prev_state ~f:(fun prev changed ->
          set changed ValueDom.top prev)
    in
    (* let () = printf "in widen_with_step: steps %d\n%!" steps in *)
    let f = if steps < widen_threshold then merge else widen_state in
    f prev_state new_state

  let pp (env : t) : unit =
    let env_entry_to_string ~(key : string) ~(data: ValueDom.t)
        : string =
      let val_str = ValueDom.to_string data in
      sprintf "%s --> %s" key val_str
    in
    printf "* Env is:\n%!";
    M.iteri env ~f:(fun ~key ~data ->
        let entry_str = env_entry_to_string ~key ~data in
        printf "\t%s\n%!" entry_str)
end

module AbstractInterpreter(N: NumericDomain)
         (R : sig type t end)
         (Rt : sig type t end)
         (Vt : sig type t end)
         (Env : MemoryT with type v := N.t
                         and type region := R.t
                         and type regions := Rt.t
                         and type valtypes := Vt.t) = struct
  module E = Env
  module StringSet = Set.Make_binable_using_comparator(String)

  module ST = struct
    include Monad.State.T1(E)(Monad.Ident)
    include Monad.State.Make(E)(Monad.Ident) 
  end
  open ST.Syntax
  
  let denote_binop (op : binop) : N.t -> N.t -> N.t =
    match op with
    | Bil.PLUS -> N.add
    | Bil.MINUS -> N.sub
    | Bil.TIMES -> N.mul
    | Bil.DIVIDE -> N.div
    | Bil.SDIVIDE -> N.sdiv
    | Bil.MOD -> N.umod
    | Bil.SMOD -> N.smod
    | Bil.LSHIFT -> N.lshift
    | Bil.RSHIFT -> N.rshift
    | Bil.ARSHIFT -> N.arshift
    | Bil.AND -> N.logand
    | Bil.OR -> N.logor
    | Bil.XOR -> N.logxor
    | Bil.EQ -> N.booleq
    | Bil.NEQ -> N.boolneq
    | Bil.LT -> N.boollt
    | Bil.LE -> N.boolle
    | Bil.SLT -> N.boolslt
    | Bil.SLE -> N.boolsle

  let denote_cast (c : cast) : int -> N.t -> N.t =
    match c with
    | Bil.UNSIGNED -> N.unsigned
    | Bil.SIGNED -> N.signed
    | Bil.HIGH -> N.high
    | Bil.LOW -> N.low

  let denote_unop (op : unop) : N.t -> N.t =
    match op with
    | Bil.NEG -> N.neg
    | Bil.NOT -> N.lnot

  let rec denote_exp (e : Bil.exp) : N.t ST.t =
    (* let () = printf "denoting exp: %a\n%!" Exp.ppo e interval_analysis.ml *)
    (* ST.get () >>= fun st -> *)
    try
      begin
        match e with
        | Bil.Load (_mem, idx, _endian, size) ->
           (* let () = Format.printf "Denoting load\n%!" in *)
           denote_exp idx >>= fun offs ->
           ST.get () >>= fun st ->
           (* let () = Format.printf "doing load in denote of load\n%!" in  *)
           let res = E.load_of_bil_exp e offs size st in
           begin
             match res with
             | Ok (res, st') ->
                (* let () = Format.printf "Done denoting load\n%!" in *)
                (* let () = Format.printf "loaded data was: %s\n%!" (N.to_string res) in *)
                ST.put st' >>= fun () ->
                ST.return res
             | Error msg -> failwith @@ Error.to_string_hum msg
           end
        
        | Bil.Store (_mem, idx, v, _endian, size) ->
           (* let () = printf "in denote_exp of store, denoting idx\n%!" in *)
           denote_exp idx >>= fun offs ->
           (* let () = printf "in denote_Exp of store, idx is: %s\n%!" *)
           (*            (N.to_string offs) *)
           (* in *)
           (* let () = printf "in denote_exp of store, size is: %a\n%!" *)
           (*            Size.ppo size *)
           (* in *)
           (* let () = printf "in denote_exp of store, denoting data\n%!" in *)
           denote_exp v >>= fun data ->
           (* let () = printf "in denote_exp of store, computing type\n%!" in *)
           begin
             (* let () = printf "in denote_exp of store, doing store\n%!" in  *)
             ST.get () >>= fun st ->
             match Env.store_of_bil_exp e ~offs ~data ~size st with
             | Ok newenv -> ST.put newenv >>= fun () -> ST.return N.bot
             | Error msg -> failwith @@ Error.to_string_hum msg
           end
        
        | Bil.BinOp (op, x, y) ->
           (* let bop_str = binop_to_string op in *)
           (* let () = Format.printf "Denoting binop %s\n%!" bop_str in *)
           denote_exp x >>= fun x' ->
           (* let () = printf "Done denoting left\n%!" in *)
           denote_exp y >>= fun y' ->
           (* let () = printf "Done denoting right\n%!" in *)
           (* let () = Format.printf "Denoting partially evald expression: %s %s %s\n%!" *)
           (* (N.to_string x') bop_str (N.to_string y') *)
           (* in *)
           ST.return @@ denote_binop op x' y' >>= fun res ->
           (* let () = Format.printf "done denoting binop %s\n%!" bop_str in *)
           ST.return res
        
        | Bil.UnOp (op, x) ->
           denote_exp x >>= fun x' ->
           ST.return @@ denote_unop op x'
        
        | Bil.Var v ->
           ST.gets @@ fun st ->
                      let name = Var.name v in
                      E.lookup name st
        
        | Bil.Int w ->
           ST.return @@ N.of_word w
        
        | Bil.Cast (cast, n, exp) ->
           denote_exp exp >>= fun exp' ->
           ST.return @@ denote_cast cast n exp'

        | Bil.Ite (cond, ifthen, ifelse) ->
           denote_exp cond >>= fun cond' ->
           let truthy = N.could_be_true cond' in
           let falsy = N.could_be_false cond' in
           if truthy && not falsy
           then denote_exp ifthen
           else
             if not truthy && falsy
             then denote_exp ifelse
             else
               denote_exp ifthen >>= fun then' ->
               denote_exp ifelse >>= fun else' ->
               ST.return @@ N.join then' else'
        
        | Bil.Unknown (str, _) ->
           (* This seems to be used for at least:
              setting undefined flags (like everything
              but OF,CF after x86_64 mul)

              also, results of cpuid/feature identification in libsodium:
              0003b294: RAX := pad:64[unknown[bits]:u32]
              0003b297: RBX := pad:64[unknown[bits]:u32]
              0003b29a: RCX := pad:64[unknown[bits]:u32]
              0003b29d: RDX := pad:64[unknown[bits]:u32]
              0003b2a9: #12582456 := RBX
              0003b2ad: RBX := RSI
              0003b2b1: RSI := #12582456
              0003b2c3: #12582455 := low:32[RAX]
              0003b2c6: OF := 0
              0003b2c9: CF := 0
              0003b2cc: AF := unknown[bits]:u1
              0003b2d1: PF :=
                        ~low:1[let $221 = #12582455 >> 4 ^ #12582455 in
                               let $222 = $221 >> 2 ^ $221 in $222 >> 1 ^ $222]
              0003b2d5: SF := high:1[#12582455]
              0003b2d9: ZF := 0 = #12582455

              for now, return top *)
           ST.return N.top
        
        | Bil.Let (var, exp, body) ->
           ST.get () >>= fun prestate -> 
           denote_exp exp >>= fun binding ->
           let name = Var.name var in
           ST.update @@ E.set name binding >>= fun _ ->
           (* todo, what if a store in body *)
           denote_exp body >>= fun v -> 
           ST.put prestate >>= fun () ->
           
           ST.return v
        | Bil.Extract (hi, lo, e) ->
           (* let () = Format.printf "Denoting extract\n%!" in  *)
           denote_exp e >>= fun e' ->
           ST.return @@ N.extract e' hi lo >>= fun res ->
           (* let () = Format.printf "Done denoting extract\n%!" in *)
           ST.return res
        
        | Bil.Concat (x, y) ->
           (* let () = Format.printf "Denoting concat\n%!" in  *)
           denote_exp x >>= fun x' ->
           denote_exp y >>= fun y' ->
           ST.return @@ N.concat x' y' >>= fun res ->
           (* let () = Format.printf "Done denoting concat\n%!" in *)
           ST.return res
      end
    with
    | Z.Overflow ->
       let err = Format.sprintf
                  "in AI.denote_elt, Z.Overflow in denoting exp: %a\n%!"
                  Exp.pps e
       in
       failwith err

  let denote_def (d : def term) : unit ST.t =
    let var = Def.lhs d in
    let varname = Var.name var in
    let rhs = Def.rhs d in
    (try denote_exp rhs with
    | Z.Overflow ->
       let elt_tid = Term.tid d in
       let e = Format.sprintf
                 "In AI.denote_def, Z.Overflow in denoting %a\n%!"
                 Tid.pps elt_tid
       in
       failwith e) >>= fun denoted_rhs ->
    ST.update @@ E.set varname denoted_rhs

  let denote_phi (p : phi term) : unit ST.t =
    ST.get () >>= fun st ->
    let _options = Phi.values p in
    let _lhs_name = Phi.lhs p |> Var.name in
    let _ = failwith "denote_phi not implemented yet" in
    ST.return ()

  let denote_jmp (j : jmp term) : unit ST.t =
    match Jmp.kind j with
    | Call c -> ST.update E.havoc_on_call
    | Goto _
    | Ret _
    | Int _ -> ST.return ()

  let denote_elt (e : Blk.elt) (st : E.t) : E.t =
    (* let () = printf "in-state is:\n%!"; E.pp st in *)
    let res = match e with
      | `Def d ->
         let () = Format.printf "Denoting tid %a\n%!" Tid.pp (Term.tid d) in
         denote_def d
      | `Jmp j ->
         let () = Format.printf "Denoting tid %a\n%!" Tid.pp (Term.tid j) in
         denote_jmp j 
      | `Phi p ->
         let () = Format.printf "Denoting tid %a\n%!" Tid.pp (Term.tid p) in
         denote_phi p in
    let (elt_res, state') = ST.run res st in
    (* let () = Format.printf "out-state is:\n%!"; E.pp state' in *)
    state'
end

module DomainProduct(X : NumericDomain)(Y : NumericDomain)
       : NumericDomain = struct
  type t = X.t * Y.t

  let key : t DomainKey.k =
    let x_n = DomainKey.name X.key in
    let y_n = DomainKey.name Y.key in 
    let n = Format.sprintf "prod-%s-%s" x_n y_n in
    DomainKey.create n

  let get k =
    match X.get k with
    | Some f -> Some (fun elt -> f (fst elt))
    | None -> match Y.get k with
              | Some f -> Some (fun elt -> f (snd elt))
              | None -> None

  let set key (x, y) newval =
    X.set key x newval, Y.set key y newval
    
  let first (prod : t) : X.t =
    match prod with
    | x, y -> x

  let second (prod : t) : Y.t =
    match prod with
    | x, y -> y
  
  let bot = X.bot, Y.bot
  let top = X.top, Y.top
  let make_top width signed = X.make_top width signed, Y.make_top width signed

  let b1 = X.b1, Y.b1
  let b0 = X.b0, Y.b0
  
  let order (x, y) (x', y') =
    let open KB.Order in
    match X.order x x', Y.order y y' with
    | EQ, EQ -> EQ
    | LT, LT -> LT
    | GT, GT -> GT
    | _ -> NC

  let compare x y : int =
    let open KB.Order in
    match order x y with
    | LT -> -1
    | EQ -> 0
    | GT -> 1
    | NC -> -1

  let equal f s =
    match order f s with
    | KB.Order.EQ -> true
    | _ -> false

  let join (x, y) (x', y') = X.join x x', Y.join y y'
  let meet (x, y) (x', y') = X.meet x x', Y.meet y y'

  (* If at least one domain says it's true, then it *could* be true *) 
  let contains (x, y) (x', y') = X.contains x x' || Y.contains y y'

  let binop xf yf =
    fun (x, y) (x', y') -> xf x x', yf y y'
                              
  let add = binop X.add Y.add
  let sub = binop X.sub Y.sub
  let mul = binop X.mul Y.mul
  let div = binop X.div Y.div
  let sdiv = binop X.sdiv Y.sdiv
  let umod = binop X.umod Y.umod
  let smod = binop X.smod Y.smod
  let lshift = binop X.lshift Y.lshift
  let rshift = binop X.rshift Y.rshift
  let arshift = binop X.arshift Y.arshift
  let logand = binop X.logand Y.logand
  let logor = binop X.logor Y.logor
  let logxor = binop X.logxor Y.logxor

  let neg (x, y) = X.neg x, Y.neg y
  let lnot (x, y) = X.lnot x, Y.lnot y

  (* If at least one domain says it's true, then it *could* be true *) 
  let bincomp xf yf =
    fun (x, y) (x', y') -> xf x x', yf y y'
  
  let booleq = bincomp X.booleq Y.booleq
  let boolneq = bincomp X.boolneq Y.boolneq
  let boollt = bincomp X.boollt Y.boollt
  let boolle = bincomp X.boolle Y.boolle
  let boolslt = bincomp X.boolslt Y.boolslt
  let boolsle = bincomp X.boolsle Y.boolsle

  (* If at least one domain says it's true, then it *could* be true *)
  (* TODO: taint could be made more precise here *) 
  let could_be_true (x, y) = X.could_be_true x || Y.could_be_true y

  let could_be_false (x, y) = X.could_be_false x || Y.could_be_false y

  let cast_or_extract xf yf = fun n (x, y) -> xf n x, yf n y
  let unsigned = cast_or_extract X.unsigned Y.unsigned
  let signed = cast_or_extract X.signed Y.signed
  let low = cast_or_extract X.low Y.low 
  let high = cast_or_extract X.high Y.high

  let extract (x, y) hi lo = X.extract x hi lo, Y.extract y hi lo
  let concat (x, y) (x', y') = X.concat x x', Y.concat y y'

  let to_string (x, y) = Format.sprintf "(%s, %s)" (X.to_string x) (Y.to_string y)
  let of_int ?(width = 64) i = X.of_int ~width i, Y.of_int ~width i
  let of_word w = X.of_word w, Y.of_word w
  let of_z ?(width = 64) z = X.of_z ~width z, Y.of_z ~width z
  let sexp_of_t (x, y) = Sexp.List [ X.sexp_of_t x; Y.sexp_of_t y ]
  let bitwidth (x, y) =
    let bw1 = X.bitwidth x in
    let bw2 = Y.bitwidth y in
    if bw1 = bw2
    then bw1
    else
      if bw1 = -1
      then bw2
      else bw1
end
