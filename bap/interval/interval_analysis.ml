open Core_kernel
open Bap.Std
open Graphlib.Std

module KB = Bap_knowledge.Knowledge
module IML = Map_lattice.Interval

type state = Interval.t IML.t

(** Transfer function related *)

let denote_binop (op : binop) : Interval.t -> Interval.t -> Interval.t =
  let open Interval in
  match op with
   | Bil.PLUS -> add
   | Bil.MINUS -> sub
   | Bil.TIMES -> mul
   | Bil.DIVIDE -> div
   | Bil.SDIVIDE -> sdiv
   | Bil.MOD -> umod
   | Bil.SMOD -> smod
   | Bil.LSHIFT -> lshift
   | Bil.RSHIFT -> rshift
   | Bil.ARSHIFT -> arshift
   | Bil.AND -> logand
   | Bil.OR -> logor
   | Bil.XOR -> logxor
   | Bil.EQ -> booleq
   | Bil.NEQ -> boolneq
   | Bil.LT -> boollt
   | Bil.LE -> boolle
   | Bil.SLT -> boolslt
   | Bil.SLE -> boolsle

let denote_cast (c : cast) : int -> Interval.t -> Interval.t =
  match c with
   | Bil.UNSIGNED -> Interval.unsigned
   | Bil.SIGNED -> Interval.signed
   | Bil.HIGH -> Interval.high
   | Bil.LOW -> Interval.low

let denote_unop (op : unop) : Interval.t -> Interval.t =
  match op with
  | Bil.NEG -> Interval.neg
  | Bil.NOT -> Interval.lnot
              
let rec denote_exp (e : Bil.exp) (d : state) : Interval.t =
  match e with
   | Bil.Load (_mem, _idx, _endian, _size) -> Interval.top
   | Bil.Store (_mem, _idx, _val, _endian, _size) -> Interval.top
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
        match IML.find d name with
        | Some intvl -> intvl
        | None -> Interval.bot
      end
   | Bil.Int w -> Interval.of_word w
   | Bil.Cast (cast, n, exp) ->
      let exp' = denote_exp exp d in
      let cast' = denote_cast cast in
      cast' n exp'
   | Bil.Ite (cond, ifthen, ifelse) ->
      let cond' = denote_exp cond d in
      if Interval.could_be_true cond'
      then denote_exp ifthen d
      else denote_exp ifelse d
   | Bil.Unknown (str, _) ->
      let () = Format.printf "Todo: support Bil.Unknown (%s, -) \n%!" str in
      Interval.top
   | Bil.Let (var, exp, body) ->
      let binding = denote_exp exp d in
      let varname = Var.name var in
      let env' = IML.set d ~key:varname ~data:binding in
      denote_exp body env'
   | Bil.Extract (_, _, _) ->
      let () = Format.printf "TODO: support Bil.Extract\n%!" in
      Interval.top
   | Bil.Concat (_, _) ->
      let () = Format.printf "TODO: support Bil.Concat\n%!" in
      Interval.top

let denote_def (d : def term) : state -> state =
  fun state ->
  let varname = Def.lhs d |> Var.name in
  let rhs = Def.rhs d in
  let denoted_rhs = denote_exp rhs state in
  IML.set state ~key:varname ~data:denoted_rhs

let denote_phi (p : phi term) : state -> state =
  fun state ->
  let () = Format.printf "TODO: write def of denote_phi\n%!" in
  state

let denote_jmp (j : jmp term) : state -> state =
  fun state -> state

let denote_blk (b : blk term) : state -> state =
  fun state ->
  Term.enum def_t b |>
    Seq.fold ~init:state ~f:(fun prev_state def ->
        (denote_def def) prev_state)

let denote_node (n : Graphs.Ir.node) : state -> state =
  Graphs.Ir.Node.label n |> denote_blk

(** Solver related *)

let default_elt_value : Interval.t = Interval.bot

let make_initial_solution (sub : sub term) (irg : Graphs.Ir.t)
    : (Graphs.Ir.node, state) Solution.t =
  let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) irg in
  let first_node = Seq.hd_exn nodes in
  let first_block = Graphs.Ir.Node.label first_node in
  let free_vars = Blk.free_vars first_block in
  let empty = IML.empty in
  let with_initial_info = Var.Set.fold free_vars
                            ~init:empty
                            ~f:(fun acc var ->
                              let key = Var.name var in
                              IML.set acc ~key ~data:Interval.top)
  in
  let with_rsp_fixed = IML.set with_initial_info
                         ~key:"RSP"
                         ~data:(Interval.of_int 0)
  in
  let empty_node_map = Graphs.Ir.Node.Map.empty in
  let first = Graphs.Ir.Node.Map.set empty_node_map
                ~key:first_node
                ~data:with_rsp_fixed
  in
  Solution.create first (IML.empty)

let run (s : sub term) : (Graphs.Ir.node, state) Solution.t =
  let irg = Sub.to_cfg s in
  let init = make_initial_solution s irg in
  let merge state1 state2 : state =
    IML.fold state2 ~init:state1 ~f:(fun ~key ~data prev ->
        if IML.mem prev key
        then
          begin
            let last = IML.find_exn prev key in
            let merged = Interval.join last data in
            IML.set prev ~key ~data:merged
          end
        else IML.set prev ~key ~data)
  in
  let widen_threshold = 256 in
  let widen steps n prev_state new_state : state =
    if steps < widen_threshold
    then merge prev_state new_state
    else
      IML.fold new_state ~init:prev_state ~f:(fun ~key ~data prev ->
          if IML.mem prev key
          then
            begin
              let prev_intvl = IML.find_exn prev key in
              let widened_val = if not (Interval.equal data prev_intvl)
                                then Interval.top
                                else data
              in
              IML.set prev ~key ~data:widened_val
            end
          else
            IML.set prev ~key ~data)
  in
  Graphlib.fixpoint
    (module Graphs.Ir)
    irg
    ~init
    ~equal:(IML.equal Interval.equal)
    ~merge
    ~f:denote_node
    ~step:widen

