open Core
open Bap.Std
open Graphlib.Std

module KB = Bap_knowledge.Knowledge
module IML = Map_lattice.Interval
module T = Bap_core_theory.Theory

type result = Wrapping_interval.t

type state = result IML.t

(** Transfer function related *)
let denote_binop (op : binop) : result -> result -> result =
  (* let open Interval in *)
  let open Wrapping_interval in
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

let denote_cast (c : cast) : int -> result -> result =
  let open Wrapping_interval in
  match c with
   | Bil.UNSIGNED -> unsigned
   | Bil.SIGNED -> signed
   | Bil.HIGH -> high
   | Bil.LOW -> low

let denote_unop (op : unop) : result -> result =
  match op with
  | Bil.NEG -> Wrapping_interval.neg
  | Bil.NOT -> Wrapping_interval.lnot
              
let rec denote_exp (e : Bil.exp) (d : state) : result =
  match e with
  | Bil.Load (_mem, _idx, _endian, size) ->
     Wrapping_interval.make_top (Size.in_bits size) false
  | Bil.Store (_mem, _idx, _val, _endian, size) ->
     Wrapping_interval.make_top (Size.in_bits size) false
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
       | None -> Wrapping_interval.bot
     end
  | Bil.Int w -> Wrapping_interval.of_word w
  | Bil.Cast (cast, n, exp) ->
     let exp' = denote_exp exp d in
     let cast' = denote_cast cast in
     cast' n exp'
  | Bil.Ite (cond, ifthen, ifelse) ->
     let cond' = denote_exp cond d in
     if Wrapping_interval.could_be_true cond'
     then denote_exp ifthen d
     else denote_exp ifelse d
  | Bil.Unknown (str, _) ->
     (* This seems to be used for at least:
        setting undefined flags (like everything
        but OF,CF after x86_64 mul) *)
     Wrapping_interval.bot
  | Bil.Let (var, exp, body) ->
     let binding = denote_exp exp d in
     let varname = Var.name var in
     let env' = IML.set d ~key:varname ~data:binding in
     denote_exp body env'
  | Bil.Extract (_, _, _) ->
     let () = Format.printf "TODO: support Bil.Extract\n%!" in
     Wrapping_interval.top
  | Bil.Concat (_, _) ->
     let () = Format.printf "TODO: support Bil.Concat\n%!" in
     Wrapping_interval.top

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

let default_elt_value : result = Wrapping_interval.bot

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
                         IML.set acc ~key ~data:Wrapping_interval.bot)
  in
  let with_free_vars_iml = Var.Set.fold free_vars
                             ~init:all_vars_iml
                             ~f:(fun acc var ->
                               let key = Var.name var in
                               IML.set acc ~key ~data:Wrapping_interval.top)
  in
  let with_args_iml  = Seq.fold argnames
                         ~init:with_free_vars_iml
                         ~f:(fun acc key ->
                           IML.set acc ~key ~data:Wrapping_interval.top)
  in
  let with_rsp_fixed = IML.set with_args_iml
                         ~key:"RSP"
                         ~data:(Wrapping_interval.of_int 0)
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
            let merged = Wrapping_interval.join last data in
            IML.set prev ~key ~data:merged
          end
        else IML.set prev ~key ~data)

(* let new_run (s : sub term) : (Graphs.Ir.node, state) Solution.t = *)
(*   let module E = Common.Env(Wrapping_interval) in *)
(*   let irg = Sub.to_cfg s in *)
(*   let init_sol = E.initial_solution sub irg in *)
(*   Graphlib.fixpoint *)
(*     (module Graphs.Ir) *)
(*     irg *)
(*     ~init:(E.initial_solution sub irg) *)
(*     ~equal:E.equal *)
(*     ~merge:E.merge *)
(*     ~f:denote_node *)
(*     ~step:E.widen_with_step *)

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
              let widened_val = if not (Wrapping_interval.equal data prev_intvl)
                                then Wrapping_interval.top
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
    ~equal:(IML.equal Wrapping_interval.equal)
    ~merge
    ~f:denote_node
    ~step:widen

