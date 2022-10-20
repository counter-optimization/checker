open Core_kernel
open Bap.Std
open Graphlib.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

module type NumericDomain = sig
  type t

  val bot : t
  val top : t
  val make_top : int -> bool -> t 

  val b1 : t
  val b0 : t

  val order : t -> t -> KB.Order.partial
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
  val of_int : int -> t
  val of_word : word -> t
  val sexp_of_t : t -> Sexp.t
end

module NumericEnv(ValueDom : NumericDomain) = struct
  module M = Map.Make_binable_using_comparator(String)
  module G = Graphlib.Make(Tid)(Unit)
    
  type t = ValueDom.t M.t
  type key = string
  type v = ValueDom.t

  let stack_ptr = "RSP"
  let frame_ptr = "RBP"

  let lookup name env =
    match M.find env name with
    | Some v -> v
    | None -> ValueDom.bot

  let set name v env : t = M.set env ~key:name ~data:v
  let mem = M.mem

  let equal = M.equal ValueDom.equal
  
  let empty : t = M.empty
  let empty_for_entry : t =
    let zero = Word.of_int ~width:64 0 in
    let abszero = ValueDom.of_word zero in
    empty
    |> set stack_ptr abszero
    |> set frame_ptr abszero

  let empty_with_args (sub : sub term) : t =
    let init = empty_for_entry in
    let args = Term.enum arg_t sub in
    let argnames = Seq.map args ~f:(fun a -> Arg.var a |> T.Var.name) in
    Seq.fold argnames ~init ~f:(fun prev name ->
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
    let first_node = Seq.hd_exn nodes in
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
    | Bil.Extract (_, _, _) ->
       let () = Format.printf "TODO: support Bil.Extract\n%!" in
       N.top
    | Bil.Concat (_, _) ->
       let () = Format.printf "TODO: support Bil.Concat\n%!" in
       N.top

  let denote_def (d : def term) : E.t -> E.t =
    fun state ->
    let varname = Def.lhs d |> Var.name in
    let rhs = Def.rhs d in
    let denoted_rhs = denote_exp rhs state in
    E.set varname denoted_rhs state

  let denote_phi (p : phi term) : E.t -> E.t =
    fun state -> failwith "denote_phi not implemented yet"

  let denote_jmp (j : jmp term) : E.t -> E.t =
    fun state -> state

  let denote_elt (e : Blk.elt) : E.t -> E.t =
    fun state ->
    match e with
    | `Def d -> denote_def d state
    | `Jmp j -> denote_jmp j state
    | `Phi p -> denote_phi p state
end

module DomainProduct(X : NumericDomain)(Y : NumericDomain) : NumericDomain = struct
  type t = X.t * Y.t
  
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

  let equal f s =
    match order f s with
    | KB.Order.EQ -> true
    | _ -> false
end
