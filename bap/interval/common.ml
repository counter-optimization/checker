open Core_kernel
open Bap.Std
open Graphlib.Std
open Monads.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

type cell_t = Scalar | Ptr [@@deriving sexp, bin_io, compare, equal]

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

module type NumericDomain = sig
  type t

  val key : t DomainKey.k
  val get : 'a DomainKey.k -> (t -> 'a) option
  val set : 'a DomainKey.k -> 'a -> t -> 'a

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

module type NumericEnvT =
  functor (N : NumericDomain) ->
  sig
    type t

    val lookup : string -> t -> N.t
    val set : string -> N.t -> t -> t
    val mem : t -> string -> bool
    val equal : t -> t -> bool
    val empty : t
    val empty_for_entry : t
    val empty_with_args : t
    val merge : t -> t -> t
    val widen_threshold : int
    val widen_with_step : int -> 'a -> t -> t -> t
    val pp : t -> unit
  end

module type MemoryT =
  sig
    type t
    type v
    type regions
    type region
    type valtypes

    val empty : t
    val lookup : string -> t -> v
    val set : string -> v -> t -> t
    val equal : t -> t -> bool

    val compute_type : Bil.exp -> t -> cell_t
    val set_rsp : int -> t -> t
    val set_rbp : int -> t -> t
    val set_img : t -> Image.t -> t
    val holds_ptr : string -> t -> bool
    val setptr : name:string -> regions:regions -> offs:v -> width:v -> t -> t
    val unptr : name:string -> t -> t
    val update_on_assn : lhs:Var.t -> rhs:Bil.exp -> t -> t
    val load_of_bil_exp : Bil.exp -> v -> t -> v
    val store_of_bil_exp : Bil.exp -> offs:v -> data:v -> valtype:cell_t -> t -> t
    
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

  let empty : t = M.empty
  let stack_ptr = "RSP"
  let frame_ptr = "RBP"
  let start_stack_addr = Word.of_int ~width:64 262144
  let x86_64_default_taint = ["RDI"; "RSI"; "RDX"; "RCX"; "R8"; "R9"]

  let holds_ptr var_name env = false

  let lookup name env =
    match M.find env name with
    | Some v -> v
    | None -> ValueDom.bot

  let compute_type e env = Scalar
  let set name v env : t = M.set env ~key:name ~data:v

  let set_rsp offs env = set "RSP" (ValueDom.of_int offs) env
  let set_rbp offs env = set "RBP" (ValueDom.of_int offs) env
  let set_img env img = env

  let setptr ~name ~regions ~offs ~width env = env
  let unptr ~name env = env
  let update_on_assn ~lhs ~rhs env = env

  let load_of_bil_exp (e : Bil.exp) _offs env = ValueDom.top
  let store_of_bil_exp (e : Bil.exp) ~offs ~data ~valtype env = env
  
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

  let widen_threshold = 256
  let widen_with_step steps n prev_state new_state : t =
    let get_differing_keys prev_state new_state =
      M.fold prev_state ~init:Seq.empty ~f:(fun ~key ~data acc ->
          let next = M.find_exn new_state key in
          if ValueDom.equal data next
          then acc
          else Seq.cons key acc)
    in
    let widen_state prev_state new_state =
      let changed_keys = get_differing_keys prev_state new_state in
      Seq.fold changed_keys ~init:prev_state ~f:(fun prev changed ->
          set changed ValueDom.top prev)
    in
    let f = if steps < widen_threshold then merge else widen_state in
    f prev_state new_state

  let pp (env : t) : unit =
    Format.printf "%a\n%!" Sexp.pp (M.sexp_of_t ValueDom.sexp_of_t env)
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

  (* let rec denote_exp (e : Bil.exp) : E.t -> (N.t, E.t) = *)
  let rec denote_exp (e : Bil.exp) : N.t ST.t =
    ST.get () >>= fun st ->
    let () = printf "in denote_exp, e is %s\n%!" @@ Exp.pps () e in
    match e with
    | Bil.Load (_mem, idx, _endian, size) ->
       denote_exp idx >>= fun offs ->
       ST.get () >>= fun st ->
       let () = printf "in bil.load denote, offs is %s\n%!" @@ N.to_string offs in
       let res = E.load_of_bil_exp e offs st in
       ST.return res
    | Bil.Store (_mem, idx, v, _endian, size) ->
       denote_exp idx >>= fun offs ->
       denote_exp v >>= fun data ->
       ST.gets (Env.compute_type v) >>= fun valtype ->
       ST.update @@ Env.store_of_bil_exp e ~offs:offs ~data ~valtype >>= fun () ->
       ST.return N.bot
    | Bil.BinOp (op, x, y) ->
       denote_exp x >>= fun x' ->
       denote_exp y >>= fun y' ->
       ST.return @@ denote_binop op x' y'
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
          but OF,CF after x86_64 mul) *)
       ST.return N.bot
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
       denote_exp e >>= fun e' ->
       ST.return @@ N.extract e' hi lo
    | Bil.Concat (x, y) ->
       denote_exp x >>= fun x' ->
       denote_exp y >>= fun y' ->
       ST.return @@ N.concat x' y'

  let denote_def (d : def term) : unit ST.t =
    let var = Def.lhs d in
    let varname = Var.name var in
    let rhs = Def.rhs d in
    
    denote_exp rhs >>= fun denoted_rhs ->
    ST.get () >>= fun st ->
    ST.update @@ E.update_on_assn ~lhs:var ~rhs >>= fun () ->
    ST.update @@ E.set varname denoted_rhs

  let denote_phi (p : phi term) : unit ST.t =
    ST.get () >>= fun st ->
    let _ = failwith "denote_phi not implemented yet" in
    ST.return ()

  let denote_jmp (j : jmp term) : unit ST.t =
    let potential_label = match Jmp.kind j with
      | Call c -> Some (Call.target c)
      | Goto l -> Some l
      | Ret l -> Some l
      | Int (n, tid) -> None
    in
    let () = match potential_label with
      | Some l ->
         let ls = Label.to_string l in
         let () = Format.printf "jmp term is : %a%! -- " Jmp.pp j in
         Format.printf "label of jmp term is : %s\n%!" ls
      | None -> ()
    in
    ST.return ()

  let denote_elt (e : Blk.elt) (st : E.t) : E.t =
    let () = printf "in-state is \n%!"; E.pp st in
    let res = match e with
      | `Def d ->
         begin
           let defs = Def.pps () d in 
           let () = printf "denoting def %s\n%!" defs in
           denote_def d
         end
      | `Jmp j -> denote_jmp j 
      | `Phi p -> denote_phi p
    in
    let (elt_res, state') = ST.run res st in
    let () = printf "out-state is \n%!"; E.pp state' in
    let () = printf "---------------------------\n" in
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

  let set : type a. a DomainKey.k -> a -> t -> a = fun k other replace ->
    match DomainKey.eq_type k key with
    | Eq -> replace
    | Neq -> other

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
