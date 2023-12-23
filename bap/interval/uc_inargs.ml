open Core_kernel
open Bap.Std

module Theory = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge
module Taint = Checker_taint.Analysis

open KB.Syntax
       
let package = Common.package

let log_prefix = sprintf "%s.uc_inargs" package
module L = struct
  include Dolog.Log
  let () = set_prefix log_prefix
end

type subname = string

module T = struct
  type t

  let cls : (t, unit) KB.Class.t =
    KB.Class.declare ~public:true
      ~package
      ~desc:"holds input state for analyzing func with subname"
      "in-state" ()

  let name_dom = KB.Domain.flat
                   ~empty:""
                   ~equal:String.equal
                   "name_dom"

  let name : (t, string) KB.slot =
    KB.Class.property ~package cls "in-state-sub-name" name_dom
end

include T

module TaintInState = struct
  type t = String.Set.t

  let log_prefix = sprintf "%s.taint" log_prefix
  module L = struct
    include Dolog.Log
    let () = set_prefix log_prefix
  end

  let taintdom =
    KB.Domain.powerset (module String.Set.Elt)
      ~inspect:String.sexp_of_t "taint-in-state-dom"

  let tainted_regs : (T.t, t) KB.slot =
    KB.Class.property ~package cls "taint-map" taintdom

  let get subname : String.Set.t =
    let args = ref String.Set.empty in
    Toplevel.exec begin
      KB.objects T.cls >>= fun instates ->
      KB.Seq.find instates ~f:(fun obj ->
        KB.collect T.name obj >>= fun name ->
        KB.return @@
        String.Caseless.equal subname name)
      >>= function
        | Some obj ->
          KB.collect tainted_regs obj >>= fun taints ->
          KB.return (args := taints)
        | None -> KB.return ()
    end;
    !args

  let config (module ABI : Abi.ABIDef) (c : Config.t) : unit =
    let build_sub_taint name taintedidxs =
      KB.Object.create T.cls >>= fun st ->
      KB.provide T.name st name >>= fun () ->
      let taintedargs = Int.Set.to_list taintedidxs
                        |> List.map ~f:ABI.arg_from_idx
                        |> List.filter ~f:Option.is_some
                        |> List.map ~f:(fun e -> Option.value_exn e)
                        |> String.Set.of_list in
      L.debug "Taint input state for %s: %s" name @@ List.to_string ~f:Fn.id @@ String.Set.to_list taintedargs;
      KB.provide tainted_regs st taintedargs
    in
    let config_secs = Config.get_secret_args c in
    String.Map.iteri config_secs ~f:(fun ~key ~data ->
      Toplevel.exec @@ build_sub_taint key data)
end

module TaintContext = struct
  module M = struct
    type t = {
      subname : String.t;
      argvals : String.Set.t;
    } [@@deriving sexp, compare]
  end

  module Cmp = struct
    include M
    include Comparator.Make(M)
  end

  include M

  module Set = Set.Make_using_comparator(Cmp)
  module Map = Map.Make_using_comparator(Cmp)

  open KB.Monad_infix

  let get_all () : Set.t =
    let all = ref Set.empty in
    Toplevel.exec begin
      KB.objects T.cls >>= fun objs ->
      KB.Seq.iter objs ~f:(fun inarg ->
        KB.collect T.name inarg >>= fun subname ->
        KB.collect TaintInState.tainted_regs inarg >>= fun argvals ->
        all := Set.add !all {subname;argvals};
        KB.return ()
      )
    end;
    !all

  let compare ~left ~right =
    if not @@ String.equal left.subname right.subname
    then KB.Order.NC
    else
      let l = left.argvals in
      let r = right.argvals in
      if String.Set.equal l r
      then KB.Order.EQ
      else if String.Set.is_subset l ~of_:r
      then KB.Order.LT
      else if String.Set.is_subset r ~of_: l
      then KB.Order.GT
      else KB.Order.NC
end

module TaintSummary = struct
  module MT = struct
    type t = {
      input : String.Set.t;
      output : String.Set.t;
    } [@@deriving sexp, compare]
  end

  include MT

  let make ~input ~output = { input; output }

  let output_subsumed left ~by =
    let l = left.output in
    let r = by.output in
    if String.Set.equal l r
    then KB.Order.EQ
    else if String.Set.is_subset l ~of_:r
    then KB.Order.LT
    else if String.Set.is_subset r ~of_: l
    then KB.Order.GT
    else KB.Order.NC

  let update_output ({input;_} : t) ~output = {input; output}

  let output ({output;_} : t) = output
  let input ({input;_} : t) = input
end

module InterprocTaintpreter = struct
  module T = Checker_taint.Analysis
  module ST = String.Set

  type state = ST.t
               
  let denote_binop (op : binop) : T.t -> T.t -> T.t =
    match op with
    | Bil.PLUS -> T.add
    | Bil.MINUS -> T.sub
    | Bil.TIMES -> T.mul
    | Bil.DIVIDE -> T.div
    | Bil.SDIVIDE -> T.sdiv
    | Bil.MOD -> T.umod
    | Bil.SMOD -> T.smod
    | Bil.LSHIFT -> T.lshift
    | Bil.RSHIFT -> T.rshift
    | Bil.ARSHIFT -> T.arshift
    | Bil.AND -> T.logand
    | Bil.OR -> T.logor
    | Bil.XOR -> T.logxor
    | Bil.EQ -> T.booleq
    | Bil.NEQ -> T.boolneq
    | Bil.LT -> T.boollt
    | Bil.LE -> T.boolle
    | Bil.SLT -> T.boolslt
    | Bil.SLE -> T.boolsle

  let denote_cast (c : cast) : int -> T.t -> T.t =
    match c with
    | Bil.UNSIGNED -> T.unsigned
    | Bil.SIGNED -> T.signed
    | Bil.HIGH -> T.high
    | Bil.LOW -> T.low

  let denote_unop (op : unop) : T.t -> T.t =
    match op with
    | Bil.NEG -> T.neg
    | Bil.NOT -> T.lnot

  let rec denote_exp (e : Bil.exp) (st : state) : (T.t * state) =
    match e with
    | Bil.Load (_, _, _, _) -> (T.Taint, st)
    | Bil.Store (_, _, _, _, _) -> (T.Notaint, st)
    | Bil.BinOp (bop, l, r) ->
      let bop = denote_binop bop in
      let (lt, st) = denote_exp l st in
      let (rt, st) = denote_exp r st in
      (bop lt rt, st)
    | Bil.UnOp (uop, l) ->
      let uop = denote_unop uop in
      let (lt, st) = denote_exp l st in
      (uop lt, st)
    | Bil.Var v ->
      let name = Var.name v in
      if String.Set.mem st name
      then T.Taint, st
      else T.Notaint, st
    | Bil.Int _ -> T.Notaint, st
    | Bil.Cast (c, s, e) ->
      let (et, st) = denote_exp e st in
      let c = denote_cast c in
      (c s et, st)
    | Bil.Let (v, bindexp, body) ->
      let bindexp, st = denote_exp bindexp st in
      let vname = Var.name v in
      let st' = if T.is_tainted bindexp
        then String.Set.add st vname
        else st in
      let (res, _) = denote_exp body st' in
      (res, st)
    | Bil.Unknown (_, _) -> T.Taint, st
    | Bil.Ite (_, then_, else_) ->
      let (then_, st) = denote_exp then_ st in
      let (else_, st) = denote_exp else_ st in
      T.join then_ else_, st
    | Bil.Extract (_, _, e) -> denote_exp e st
    | Bil.Concat (l, r) ->
      let (lt, st) = denote_exp l st in
      let (rt, st) = denote_exp r st in
      (T.join lt rt, st)

  let denote_def (proj : Project.t) (d : def term) (st : state) : state =
    let var = Def.lhs d in
    let varname = Var.name var in
    let rhs = Def.rhs d in
    let result, st = denote_exp rhs st in
    if T.is_tainted result
    then String.Set.add st varname
    else st

  let denote_phi (proj : Project.t) (p : phi term) (st : state) : state =
    failwith "denote_phi not implemented yet InterprocTaintpreter"

  let denote_jmp (proj : Project.t) (j : jmp term) (st : state) : state =
    match Jmp.kind j with
    | Call c -> begin match Call.target c with
      | Direct calleetid ->
        L.debug "Trying to find callee sub with tid: %a" Tid.ppo calleetid;
        let prog = Project.program proj in
        (match Term.find sub_t prog calleetid with
         | Some sub ->
           L.debug "Found callee sub: (%a, %s)"
             Tid.ppo calleetid @@ Sub.name sub;
           st
         | None -> st)
      | Indirect exp -> st
    end
    | _ -> st

  let denote_elt (proj : Project.t) (e : Blk.elt) (st : state) : state =
    match e with
    | `Def d -> denote_def proj d st
    | `Jmp j -> denote_jmp proj j st
    | `Phi p -> denote_phi proj p st
end
