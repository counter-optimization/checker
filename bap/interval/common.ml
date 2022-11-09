open Core
open Bap.Std
open Graphlib.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

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

module NumericEnv(ValueDom : NumericDomain) = struct
  module M = Map.Make_binable_using_comparator(String)
  module G = Graphlib.Make(Tid)(Unit)
    
  type v = ValueDom.t
  type t = ValueDom.t M.t

  let stack_ptr = "RSP"
  let frame_ptr = "RBP"
  let start_stack_addr = Word.of_int ~width:64 262144
  let x86_64_default_taint = ["RDI"; "RSI"; "RDX"; "RCX"; "R8"; "R9"]

  let lookup name env =
    match M.find env name with
    | Some v -> v
    | None -> ValueDom.bot

  let set name v env : t = M.set env ~key:name ~data:v
  
  let mem = M.mem

  let equal = M.equal ValueDom.equal
  
  let empty : t = M.empty
  
  let empty_for_entry : t =
    let stack_start = ValueDom.of_word start_stack_addr in
    empty
    |> set stack_ptr stack_start
    |> set frame_ptr stack_start

  let empty_with_args (sub : sub term) : t =
    let init = empty_for_entry in
    
    let args = Term.enum arg_t sub in
    let argnames = Seq.map args ~f:(fun a -> Arg.var a |> T.Var.name) |> Seq.to_list in
    
    let frees = Sub.free_vars sub in
    let freenames = Set.to_list frees |> List.map ~f:Var.name in
    let is_arg arg = List.mem x86_64_default_taint arg ~equal:String.equal in
    let actual_args = List.filter freenames ~f:is_arg in
    
    let all_args = List.append actual_args argnames in
    List.fold all_args ~init ~f:(fun prev name ->
        set name ValueDom.top prev)

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

  let initial_solution (sub : sub term) (cfg : G.t) : (G.node, ValueDom.t M.t) Solution.t =
    let nodes = Graphlib.reverse_postorder_traverse (module G) cfg in
    let first_node = match Seq.hd nodes with
      | Some n -> n
      | None -> failwith "in Common.Env.initial_solution, couldn't get first node"
    in
    let entry_env = empty_with_args sub in
    let with_args = G.Node.Map.empty |>
                      G.Node.Map.set ~key:first_node ~data:entry_env in
    Solution.create with_args empty

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

(* module type AbstractInterpreterT *)

module AbstractInterpreter(N: NumericDomain) = struct
  module E = NumericEnv(N)

  type expr_t = N.t

  let denote_binop (op : binop) : expr_t -> expr_t -> expr_t =
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

  let denote_cast (c : cast) : int -> expr_t -> expr_t =
    match c with
    | Bil.UNSIGNED -> N.unsigned
    | Bil.SIGNED -> N.signed
    | Bil.HIGH -> N.high
    | Bil.LOW -> N.low

  let denote_unop (op : unop) : expr_t -> expr_t =
    match op with
    | Bil.NEG -> N.neg
    | Bil.NOT -> N.lnot

  let rec denote_exp (e : Bil.exp) (d : E.t) : expr_t =
    match e with
    | Bil.Load (_mem, _idx, _endian, size) ->
       N.make_top (Size.in_bits size) false
    | Bil.Store (_mem, _idx, _val, _endian, size) ->
       N.make_top (Size.in_bits size) false
    | Bil.BinOp (op, x, y) ->
       let x' = denote_exp x d in
       let y' = denote_exp y d in
       (denote_binop op) x' y'
    | Bil.UnOp (op, x) ->
       let x' = denote_exp x d in
       (denote_unop op) x'
    | Bil.Var v ->
       begin
         let name = Var.name v in
         E.lookup name d
       end
    | Bil.Int w -> N.of_word w
    | Bil.Cast (cast, n, exp) ->
       let exp' = denote_exp exp d in
       let cast' = denote_cast cast in
       cast' n exp'
    | Bil.Ite (cond, ifthen, ifelse) ->
       let cond' = denote_exp cond d in
       let truthy = N.could_be_true cond' in
       let falsy = N.could_be_false cond' in
       let compute_then = fun () -> denote_exp ifthen d in
       let compute_else = fun () -> denote_exp ifelse d in
       if truthy && not falsy
       then compute_then ()
       else
         if not truthy && falsy
         then compute_else ()
         else N.join (compute_then ()) (compute_else ())
    | Bil.Unknown (str, _) ->
       (* This seems to be used for at least:
          setting undefined flags (like everything
          but OF,CF after x86_64 mul) *)
       N.bot
    | Bil.Let (var, exp, body) ->
       let binding = denote_exp exp d in
       let name = Var.name var in
       let env' = E.set name binding d in
       denote_exp body env'
    | Bil.Extract (hi, lo, e) ->
       let e' = denote_exp e d in 
       N.extract e' hi lo
    | Bil.Concat (x, y) ->
       let x' = denote_exp x d in
       let y' = denote_exp y d in
       N.concat x' y'

  let denote_def (d : def term) : E.t -> E.t =
    fun state ->
    let varname = Def.lhs d |> Var.name in
    let rhs = Def.rhs d in
    let denoted_rhs = denote_exp rhs state in
    E.set varname denoted_rhs state

  let denote_phi (p : phi term) : E.t -> E.t =
    fun state -> failwith "denote_phi not implemented yet"

  let denote_jmp (j : jmp term) : E.t -> E.t =
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
    fun state -> state

  let denote_elt (e : Blk.elt) : E.t -> E.t =
    fun state ->
    match e with
    | `Def d -> denote_def d state
    | `Jmp j -> denote_jmp j state
    | `Phi p -> denote_phi p state
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
