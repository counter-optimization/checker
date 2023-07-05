open Core
open Bap.Std
open Bap_primus.Std
open Graphlib.Std
open Monads.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

module type NumericDomain = sig
  type t

  val key : t Domain_key.DomainKey.k
  val get : 'a Domain_key.DomainKey.k -> (t -> 'a) option
  val set : 'a Domain_key.DomainKey.k -> t -> 'a -> t

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

    val init_arg : name:string -> Config.t -> sub term -> t -> t
    
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

    val store_global : addr:addr -> data:word -> valtype:Common.CellType.t -> t -> t err

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

  let init_arg ~(name : string) _config _sub env : t = M.set env ~key:name ~data:ValueDom.top

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

  let widen_threshold = Common.ai_widen_threshold
  
  let widen_with_step steps n prev_state new_state : t =
    let get_differing_keys prev_state new_state =
      M.fold prev_state ~init:Seq.empty ~f:(fun ~key ~data acc ->
          let next = M.find_exn new_state key in
          if ValueDom.equal data next
          then
            acc
          else
            Seq.cons key acc)
    in
    let widen_state prev_state new_state =
      let changed_keys = get_differing_keys prev_state new_state in
      Seq.fold changed_keys ~init:prev_state ~f:(fun prev changed ->
          set changed ValueDom.top prev)
    in
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

module DomainProduct(X : NumericDomain)(Y : NumericDomain)
       : NumericDomain = struct
  type t = X.t * Y.t

  let key : t Domain_key.DomainKey.k =
    let x_n = Domain_key.DomainKey.name X.key in
    let y_n = Domain_key.DomainKey.name Y.key in 
    let n = Format.sprintf "prod-%s-%s" x_n y_n in
    Domain_key.DomainKey.create n

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

module AbstractInterpreter(N: NumericDomain)
         (R : sig type t end)
         (Rt : sig type t end)
         (Vt : sig type t end)
         (Env : MemoryT with type v := N.t
                         and type region := R.t
                         and type regions := Rt.t
                         and type valtypes := Vt.t) = struct
  module E = Env
  module StringSet = Common.SS

  let get_intvl : N.t -> Wrapping_interval.t =
      match N.get Wrapping_interval.key with
      | Some f -> f
      | None -> failwith "Couldn't extract interval information out of product domain during analysis"
  
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

  let rec denote_exp (e : Bil.exp) (st : E.t) : N.t * E.t =
    try
      begin
        let () = printf "AbsInt denoting exp: %s\n%!" @@
                   Common.exp_to_string e
        in
        let res = 
        match e with
        | Bil.Load (_mem, idx, _endian, size) ->
           let (offs, st) = denote_exp idx st in
           begin
             match E.load_of_bil_exp e offs size st with
             | Ok res -> res
             | Error msg -> failwith @@ Error.to_string_hum msg
           end
        | Bil.Store (_mem, idx, v, _endian, size) ->
           let offs, st = denote_exp  idx st in
           let data, st = denote_exp  v st in
           begin
             match Env.store_of_bil_exp e ~offs ~data ~size st with
             | Ok st -> (N.bot, st)
             | Error msg -> failwith @@ Error.to_string_hum msg
           end
        | Bil.BinOp (op, x, y) ->
           let (x', st) = denote_exp x st in
           let (y', st) = denote_exp y st in
           (denote_binop op x' y', st)
        | Bil.UnOp (op, x) ->
           let (x', st) = denote_exp x st in
           (denote_unop op x', st)
        | Bil.Var v ->
           let name = Var.name v in
           (E.lookup name st, st)
        | Bil.Int w ->
           (N.of_word w, st)
        | Bil.Cast (cast, n, exp) ->
           let (exp', st) = denote_exp  exp st in
           (denote_cast cast n exp', st)
        | Bil.Ite (cond, ifthen, ifelse) ->
           let (cond', st) = denote_exp  cond st in
           let truthy = N.could_be_true cond' in
           let falsy = N.could_be_false cond' in
           if truthy && not falsy
           then denote_exp  ifthen st
           else
             if not truthy && falsy
             then denote_exp  ifelse st
             else
               let (then', st) = denote_exp  ifthen st in
               let (else', st) = denote_exp  ifelse st in
               (N.join then' else', st)
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
           (N.bot, st)
        | Bil.Let (var, exp, body) ->
           let prestate = st in
           let (binding, st) = denote_exp  exp st in
           let name = Var.name var in
           let st = E.set name binding st in
           (* todo, what if a store in body *)
           let (v, _) = denote_exp  body st in
           (v, prestate)
        | Bil.Extract (hi, lo, e) ->
           let (e', st) = denote_exp  e st in
           (N.extract e' hi lo, st)
        | Bil.Concat (x, y) ->
           let (x', st) = denote_exp  x st in
           let (y', st) = denote_exp  y st in
           (N.concat x' y', st)
        in
        let () = printf "Result is %s\n%!" @@
                   N.to_string @@ fst res
        in
        res
      end
    with
    | Z.Overflow ->
       let err = Format.sprintf
                  "in AI.denote_elt, Z.Overflow in denoting exp: %a\n%!"
                  Exp.pps e
       in
       failwith err

  let denote_def (subname : string) (d : def term) (st : E.t) : E.t  =
    let var = Def.lhs d in
    let varname = Var.name var in
    let rhs = Def.rhs d in
    let (denoted_rhs, st) = begin
        try denote_exp rhs st with
        | Z.Overflow ->
           let elt_tid = Term.tid d in
           let e = Format.sprintf
                     "In AI.denote_def, Z.Overflow in denoting %a\n%!"
                     Tid.pps elt_tid
           in
           failwith e
      end
    in
    E.set varname denoted_rhs st

  let denote_phi (subname : string) (p : phi term) (st : E.t) : E.t =
    failwith "denote_phi not implemented yet"

  let denote_jmp (subname : string) (j : jmp term) (st : E.t) : E.t =
    match Jmp.kind j with
    | Call c -> E.havoc_on_call st
       (* let target_label = Call.target c in *)
       (* match target_label with *)
       (* | Direct totid -> _ *)
       (* | Indirect exp -> *)
       (*    let jmp_target = denote_exp subname tid in *)
    | Goto _
    | Ret _
    | Int _ -> st

  let denote_elt (subname : string) (e : Blk.elt) (st : E.t) : E.t =
    match e with
    | `Def d -> denote_def subname d st
    | `Jmp j -> denote_jmp subname j st
    | `Phi p -> denote_phi subname p st
    (* let (elt_res, state') = ST.run res st in *)
    (* state' *)
end
