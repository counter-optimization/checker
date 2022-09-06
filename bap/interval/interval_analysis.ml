open Core_kernel
open Bap.Std
open Graphlib.Std

module KB = Bap_knowledge.Knowledge
module IML = Map_lattice.Interval
module T = Bap_core_theory.Theory

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
  let open Interval in
  match c with
   | Bil.UNSIGNED -> unsigned
   | Bil.SIGNED -> signed
   | Bil.HIGH -> high
   | Bil.LOW -> low

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
      (* This seems to be used for at least:
         setting undefined flags (like everything
           but OF,CF after x86_64 mul) *)
      Interval.bot
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
  fun state ->
  state

let denote_blk (b : blk term) : state -> state =
  fun state ->
  Term.enum def_t b |>
    Seq.fold ~init:state ~f:(fun prev_state def ->
        (denote_def def) prev_state)

let denote_node (n : Graphs.Ir.node) : state -> state =
  Graphs.Ir.Node.label n |> denote_blk

(** Solver related *)

let default_elt_value : Interval.t = Interval.bot

let setup_initial_state (sub : sub term) (irg : Graphs.Ir.t)
    : (Graphs.Ir.node, state) Solution.t =
  let module Names = Var_name_scraper.VarName in
  
  let args = Term.enum arg_t sub in
  let argnames = Seq.map args ~f:(fun arg -> Arg.var arg |> T.Var.name) in
  
  let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) irg in
  let first_node = Seq.hd_exn nodes in
  let first_block = Graphs.Ir.Node.label first_node in
  let first_blk_denotation = denote_blk first_block in

  let free_vars = Blk.free_vars first_block in

  let all_vars_in_cfg = Var_name_scraper.get_all_vars irg in
  
  let empty = IML.empty in

  let all_vars_iml = Names.Set.fold all_vars_in_cfg
                       ~init:empty
                       ~f:(fun acc key ->
                         IML.set acc ~key ~data:Interval.bot)
  in
  let with_free_vars_iml = Var.Set.fold free_vars
                             ~init:all_vars_iml
                             ~f:(fun acc var ->
                               let key = Var.name var in
                               IML.set acc ~key ~data:Interval.top)
  in
  let with_args_iml  = Seq.fold argnames
                         ~init:with_free_vars_iml
                         ~f:(fun acc key ->
                           IML.set acc ~key ~data:Interval.top)
  in
  let with_rsp_fixed = IML.set with_args_iml
                         ~key:"RSP"
                         ~data:(Interval.of_int 0)
  in
  let manual_run_first_blk = first_blk_denotation with_rsp_fixed in
  let node_iml_map = Graphs.Ir.Node.Map.empty
                     |> Graphs.Ir.Node.Map.set
                          ~key:first_node
                          ~data:manual_run_first_blk
  in
  let final_node_iml_map = Seq.fold
                             nodes
                             ~init:node_iml_map
                             ~f:(fun acc key ->
                               Graphs.Ir.Node.Map.set
                                 acc
                                 ~key
                                 ~data:all_vars_iml) |>
                             Graphs.Ir.Node.Map.set
                               ~key:first_node
                               ~data:manual_run_first_blk
  in
  Solution.create final_node_iml_map all_vars_iml

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

let run (s : sub term) : (Graphs.Ir.node, state) Solution.t =
  let irg = Sub.to_cfg s in
  let init = setup_initial_state s irg in
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
            IML.set prev ~key ~data) in
  Graphlib.fixpoint
    (module Graphs.Ir)
    irg
    ~init
    ~equal:(IML.equal Interval.equal)
    ~merge
    ~f:denote_node
    ~step:widen

