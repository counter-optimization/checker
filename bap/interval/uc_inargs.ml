open Core_kernel
open Bap.Std
open Graphlib.Std

module Theory = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge
open KB.Syntax
module Taint = Checker_taint.Analysis

let package = Common.package

let log_prefix = sprintf "%s.uc_inargs" package
module L = struct
  include Dolog.Log
  let () = set_prefix log_prefix
end

type subname = string

module ABI = Abi.AMD64SystemVABI

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
    KB.Class.property ~public:true ~package
      cls "tainted-regs" taintdom

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

  let make ~subname ~argvals : t = {subname;argvals}

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

  let of_state (name : string) (env : String.Set.t) : t =
    let args = ABI.gpr_arg_names |> String.Set.of_list in
    let argvals = String.Set.inter env args in
    make ~subname:name ~argvals

  type exn += SubNotFound of string
      
  let sub (proj : Project.t) (ctxt : t) : sub term =
    let prog = Project.program proj in
    let matches = Term.enum sub_t prog
                  |> Seq.filter ~f:(fun sub ->
                    let cur_name = Sub.name sub in
                    String.Caseless.equal ctxt.subname cur_name)
    in
    if Seq.is_empty matches
    then raise (SubNotFound ctxt.subname)
    else Seq.hd_exn matches
end

module TaintSummary = struct
  module MT = struct
    type t = {
      input : String.Set.t;
      output : String.Set.t;
    } [@@deriving sexp, compare]

    let equal (x : t) (y : t) : bool =
      compare x y = 0
  end

  include MT

  let make ~input ~output = { input; output }

  let default =
    let top_input = ABI.gpr_arg_names |> String.Set.of_list in
    let empty = String.Set.empty in
    make ~input:top_input ~output:empty

  let output_subsumed left ~by =
    let l = left.output in
    let r = by.output in
    String.Set.equal l r || String.Set.is_subset l ~of_:r
    (* if String.Set.equal l r  *)
    (* then KB.Order.EQ *)
    (* else if String.Set.is_subset l ~of_:r *)
    (* then KB.Order.LT *)
    (* else if String.Set.is_subset r ~of_: l *)
    (* then KB.Order.GT *)
    (* else KB.Order.NC *)

  let update_output ({input;_} : t) ~output = {input; output}

  let output ({output;_} : t) = output
  let input ({input;_} : t) = input

  let merge_summaries ~(old_ : t) ~(new_ : t) : t =
    let join = String.Set.union in
    { new_ with output = join old_.output new_.output }

  let dom = KB.Domain.flat
                ~inspect:MT.sexp_of_t
                ~equal:MT.equal
                ~empty:(make ~input:String.Set.empty
                          ~output:String.Set.empty)
                "flat-interproc-analysis-summary-dom"
end

module InterprocState = struct
  type t = {
    worklist : TaintContext.Set.t;
    analyzing : TaintContext.Set.t;
    results : TaintSummary.t TaintContext.Map.t;
    callers : TaintContext.Set.t TaintContext.Map.t;
  } [@@deriving sexp, compare]

  let empty () = {
    worklist = TaintContext.get_all ();
    analyzing = TaintContext.Set.empty;
    results = TaintContext.Map.empty;
    callers = TaintContext.Map.empty;
  }

  let init (st : t) : t =
    let results = TaintContext.Set.fold st.worklist
                    ~init:st.results
                    ~f:(fun results ctxt ->
                      TaintContext.Map.set results
                        ~key:ctxt
                        ~data:TaintSummary.default) in
    { st with results }
end

module InterprocTaintpreter = struct
  
end

module Analyzer = struct
  module G = Graphlib.Make(Tid)(Uc_graph_builder.ExpOpt)

  module T = Checker_taint.Analysis
  module ST = String.Set

  type env = ST.t [@@deriving sexp, compare, equal]

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

  let rec denote_exp (e : Bil.exp) (st : env) : (T.t * env) =
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

  let denote_def (proj : Project.t) (st : InterprocState.t) (ctxt : TaintContext.t) (d : def term) (env : env)
    : env * InterprocState.t =
    let var = Def.lhs d in
    let varname = Var.name var in
    let rhs = Def.rhs d in
    let result, env' = denote_exp rhs env in
    if T.is_tainted result
    then String.Set.add env' varname, st
    else env, st

  let denote_phi (proj : Project.t) (st : InterprocState.t) (ctxt : TaintContext.t) (p : phi term) (env : env)
    : env * InterprocState.t =
    L.error "denote_phi not implemented yet InterprocTaintpreter";
    env, st

  type exn += CalleeNotFound of Tid.t
  
      
  let rec analyze_ctxt
            (proj : Project.t)
            (st : InterprocState.t)
            (ctxt : TaintContext.t)
    : TaintSummary.t * InterprocState.t =
    let get_result (st : InterprocState.t) ctxt = match TaintContext.Map.find st.results ctxt with
      | Some summary -> summary
      | None -> TaintSummary.default
    in
    let add_analyzing (st : InterprocState.t) ctxt
      : InterprocState.t =
      { st with
        analyzing = TaintContext.Set.add st.analyzing ctxt }
    in
    let remove_analyzing (st : InterprocState.t) ctxt
      : InterprocState.t =
      { st with
        analyzing = TaintContext.Set.remove st.analyzing ctxt }
    in
    let set_result (st : InterprocState.t) ctxt summary =
      {st with results = TaintContext.Map.set st.results
                           ~key:ctxt
                           ~data:summary }
    in
    let update_worklist (st : InterprocState.t) ctxt =
      let join = TaintContext.Set.union in
      let callers = match TaintContext.Map.find st.callers ctxt with
        | Some callers -> callers
        
        | None -> TaintContext.Set.empty in      
      { st with
        worklist = join st.worklist callers }
    in
    let prev_output = get_result st ctxt in
    let st = add_analyzing st ctxt in
    let sub = TaintContext.sub proj ctxt in
    let new_output, st = intraproc_propagate proj st ctxt sub in
    let st = remove_analyzing st ctxt in
    let res_subsumed = TaintSummary.output_subsumed new_output ~by:prev_output in
    if res_subsumed
    then new_output, st
    else
      let result' = TaintSummary.merge_summaries
                      ~old_:prev_output
                      ~new_:new_output in
      let st = set_result st ctxt result' in
      let st = update_worklist st ctxt in
      result', st
  and results_for
        (proj : Project.t)
        (st : InterprocState.t)
        (ctxt : TaintContext.t)
        (sub : sub term) : TaintSummary.t * InterprocState.t =
    let find_sub_result
          (st : InterprocState.t)
          (ctxt : TaintContext.t)
      : (TaintContext.t * TaintSummary.t) option =
      TaintContext.Map.fold st.results ~init:None
        ~f:(fun ~key ~data res ->
          match res with
          | (Some r) as res -> res
          | None ->
            if String.equal ctxt.subname key.subname
            then Some (key, data)
            else res)
    in
    let prev_res_subsumes_cur_ctxt
          (res : TaintSummary.t)
          (ctxt : TaintContext.t) : bool =
      let last_in_env = res.input in
      let this_in_env = ctxt.argvals in
      String.Set.is_subset this_in_env ~of_:last_in_env
    in
    let currently_analyzing_sub
          (st : InterprocState.t)
          (ctxt : TaintContext.t) : bool =
      TaintContext.Set.exists st.analyzing
        ~f:(fun analyzing_ctxt ->
          String.equal analyzing_ctxt.subname ctxt.subname)
    in
    let prev_res = find_sub_result st ctxt in
    let output, results = match prev_res with
      | Some (prevctxt, summ) when prev_res_subsumes_cur_ctxt summ ctxt ->
        Some summ.output, st.results
      | Some (prevctxt, summ) ->
        let new_input = String.Set.union summ.input ctxt.argvals in
        let new_res = { summ with input = new_input } in
        None, TaintContext.Map.set st.results
                ~key:prevctxt
                ~data:new_res
      | None ->
        let new_res = TaintSummary.make
                        ~input:ctxt.argvals
                        ~output:String.Set.empty in
        None, TaintContext.Map.set st.results
                ~key:ctxt
                ~data:new_res
    in
    let st = { st with results } in
    match output with
    | Some prev_output ->
      let prev_res = TaintSummary.make
                       ~input:ctxt.argvals
                       ~output:prev_output in
      (prev_res, st)
    | None when currently_analyzing_sub st ctxt ->
      begin match find_sub_result st ctxt with
      | Some (other_ctxt, res) -> (res, st)
      | None -> failwith "shouldn't happen"
      end
    | None -> intraproc_propagate proj st ctxt sub
  and intraproc_propagate
        (proj : Project.t)
        (st : InterprocState.t)
        (ctxt : TaintContext.t)
        (sub : sub term) : TaintSummary.t * InterprocState.t =
    let subname = Sub.name sub in
    let cfg = Uc_preanalyses.get_final_edges subname
              |> Uc_preanalyses.cfg_of_edges in
    let tidmap = Uc_preanalyses.get_tidmap subname in
    let init_map = Tid.Map.empty
                   |> Tid.Map.set
                        ~key:Uc_graph_builder.entrytid
                        ~data:ctxt.argvals in
    let init_sol = Solution.create init_map String.Set.empty in
    let st : InterprocState.t ref = ref st in
    let analysis_results = Graphlib.fixpoint
                             (module G)
                             cfg
                             ~init:init_sol
                             ~equal:String.Set.equal
                             ~start:Uc_graph_builder.entrytid
                             ~merge:String.Set.union
                             ~f:(fun tid env ->
                               let elt = match Tid.Map.find tidmap tid with
                                 | Some e -> e
                                 | None -> failwith @@
                                   sprintf "Couldn't find elt for tid %a" Tid.pps tid in
                               
                               let env', st' = denote_elt proj !st ctxt elt env in
                               st := st';
                               env')
    in
    let final_state = Solution.get analysis_results Uc_graph_builder.exittid
    in
    let result = TaintSummary.make
                   ~input:ctxt.argvals
                   ~output:final_state
    in
    result, !st
  and denote_jmp
        (proj : Project.t)
        (st : InterprocState.t)
        (curctxt : TaintContext.t)
        (j : jmp term)
        (env : env) : env * InterprocState.t =
    let add_caller
          (st : InterprocState.t)
          (callee_ctxt : TaintContext.t) : InterprocState.t =
      let cur_callers = match TaintContext.Map.find st.callers callee_ctxt with
        | Some cs -> cs
        | None -> TaintContext.Set.empty in
      let with_call = TaintContext.Set.add cur_callers curctxt in
      let callers = TaintContext.Map.set st.callers
                      ~key:callee_ctxt
                      ~data:with_call in
      { st with callers }
    in
    match Jmp.kind j with
    | Call c -> begin match Call.target c with
      | Direct calleetid ->
        L.debug "Trying to find callee sub with tid: %a" Tid.ppo calleetid;
        let callee_sub = match Common.sub_of_tid proj calleetid with
          | Some calleesub ->
            L.debug "found sub: %s" @@ Sub.name calleesub;
            calleesub
          | None -> raise (CalleeNotFound calleetid)
        in
        Uc_preanalyses.init callee_sub;
        let callee_name = Sub.name callee_sub in
        let callee_ctxt = TaintContext.of_state callee_name env in
        let prev_res = match TaintContext.Map.find st.results callee_ctxt with
          | Some res -> res
          | None -> TaintSummary.default
        in
        let cur_res, st = results_for proj st callee_ctxt callee_sub in
        let st = add_caller st callee_ctxt in
        let env' = String.Set.union prev_res.output cur_res.output
                   |> String.Set.union env
        in
        env', st
      | Indirect exp -> env, st
    end
    | _ -> env, st
  and denote_elt
        (proj : Project.t)
        (st : InterprocState.t)
        (ctxt : TaintContext.t)
        (e : Blk.elt)
        (env : env) : env * InterprocState.t =
    match e with
    | `Def d -> denote_def proj st ctxt d env
    | `Jmp j -> denote_jmp proj st ctxt j env
    | `Phi p -> denote_phi proj st ctxt p env
end

