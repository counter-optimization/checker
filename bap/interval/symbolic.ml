open Core
open Bap.Std
open Monads.Std
open Common
open Z3

module ABI = Common.ABI

module Bitv = Z3.BitVector
module Expr = Z3.Expr
module Bool = Z3.Boolean
module Solver = Z3.Solver

let ctxt = Z3.mk_context [
                 "model", "true";
                 (* "timeout", "200" (* in unsigned ms *) *)
               ]

let solver = Solver.mk_solver_s ctxt "QF_BV"

module Executor = struct
  type constraints = Expr.expr list

  type state = {
      constraints : constraints;
      fresh_var_idx : int;
      symbolic_var_map : (string, string) List.Assoc.t;
      env : (string, Expr.expr) List.Assoc.t;
      was_load : bool;
      last_loaded_symname : string option; 
      defs : def term list;
      target_tid : tid;
      do_check : bool;
      do_ss : bool;
      do_cs : bool;
      failed_ss : bool;
      failed_cs_left : bool;
      failed_cs_right : bool
    }

  type t
  
  module T = struct
    type t = state
  end
    
  module ST = struct
    include Monad.State.T1(T)(Monad.Ident)
    include Monad.State.Make(T)(Monad.Ident)
  end
  open ST.Syntax

  let init ?(do_ss : bool = false)
        ?(do_cs : bool = false)
        defs target_tid = {
      defs;
      target_tid;
      fresh_var_idx = 0;
      last_loaded_symname = None;
      was_load = false;
      symbolic_var_map = [];
      env = [];
      constraints = [];
      do_check = false;
      do_ss;
      do_cs;
      failed_ss = false;
      failed_cs_left = false;
      failed_cs_right = false
    }

  let coerce_shift op ctxt x y =
    let sx = Bitv.get_size (Expr.get_sort x)
    and sy = Bitv.get_size (Expr.get_sort y) in
    if sx = sy then op ctxt x y
    else
      let y = Bitv.mk_zero_ext ctxt (sx-sy) y in
      op ctxt x y

  let z3_of_binop : binop -> _ = function
    | PLUS -> Bitv.mk_add
    | MINUS -> Bitv.mk_sub
    | TIMES -> Bitv.mk_mul
    | DIVIDE -> Bitv.mk_udiv
    | SDIVIDE -> Bitv.mk_sdiv
    | MOD -> Bitv.mk_srem
    | SMOD -> Bitv.mk_urem
    | LSHIFT -> coerce_shift Bitv.mk_shl
    | RSHIFT -> coerce_shift Bitv.mk_lshr
    | ARSHIFT -> coerce_shift Bitv.mk_ashr
    | AND -> Bitv.mk_and
    | OR -> Bitv.mk_or
    | XOR -> Bitv.mk_xor
    | LT -> Bitv.mk_ult
    | LE -> Bitv.mk_ule
    | SLT -> Bitv.mk_slt
    | SLE -> Bitv.mk_sle
    | EQ -> fun ctxt x y ->
      Bitv.mk_not ctxt @@
      Bitv.mk_redor ctxt @@
      Bitv.mk_xor ctxt x y
    | NEQ -> fun ctxt x y ->
      Bitv.mk_redor ctxt @@
        Bitv.mk_xor ctxt x y

  let z3_of_unop : unop -> _ = function
    | NEG -> Bitv.mk_neg
    | NOT -> Bitv.mk_not

  let do_simpl x = try Expr.simplify x None with
    | Z3.Error "canceled" -> x

  let bit0 = Expr.mk_numeral_int ctxt 0 (Bitv.mk_sort ctxt 1)
  
  let bit1 = Expr.mk_numeral_int ctxt 1 (Bitv.mk_sort ctxt 1)

  let is_bool = Bool.is_bool

  let bit_of_bool x =
    if Bool.is_bool x
    then do_simpl @@ Bool.mk_ite ctxt x bit1 bit0
    else x

  let bool_of_bit x =
    do_simpl @@ Bool.mk_eq ctxt x bit1

  let coerce_to_bit x =
    if is_bool x then bit_of_bool x else x

  let coerce_to_bool x =
    if is_bool x then x else bool_of_bit x


  let simpl x = do_simpl x |>
                coerce_to_bit

  let binop op x y = z3_of_binop op ctxt x y
  
  let unop op x = z3_of_unop op ctxt x

  let word x =
    let s = Bitv.mk_sort ctxt (Word.bitwidth x) in
    let x = Word.to_bitvec x in
    if Bitvec.fits_int x
    then Expr.mk_numeral_int ctxt (Bitvec.to_int x) s
    else
      let x = Bitvec.to_bigint x in
      Expr.mk_numeral_string ctxt (Z.to_string x) s

  let extract hi lo x =
    let xs = Bitv.get_size (Expr.get_sort x)
    and ns = hi-lo+1 in
    if ns > xs
    then if lo = 0
      then Bitv.mk_zero_ext ctxt (ns-xs) x
      else
        Bitv.mk_extract ctxt hi lo @@
        Bitv.mk_zero_ext ctxt (ns-xs) x
    else
      Bitv.mk_extract ctxt hi lo x

  let concat x y = Bitv.mk_concat ctxt x y

  let ite c x y = Bool.mk_ite ctxt (coerce_to_bool c) x y

  let cast (c:cast) s x =
    let old = Bitv.get_size (Expr.get_sort x) in
    match c with
    | SIGNED -> if old < s
      then Bitv.mk_sign_ext ctxt (s-old) x
      else extract (s-1) 0 x
    | UNSIGNED -> if old < s
      then Bitv.mk_zero_ext ctxt (s-old) x
      else extract (s-1) 0 x
    | HIGH -> extract (old-1) (old-s) x
    | LOW -> extract (s-1) 0 x

  let var name size =
    Expr.mk_const_s ctxt name (Bitv.mk_sort ctxt size)

  let find_symbolic_name for_var : string option ST.t =
    ST.gets @@ fun st ->
    let equal = String.equal in
    List.Assoc.find st.symbolic_var_map ~equal for_var

  let make_fresh_symbolic : string ST.t =
    ST.get () >>= fun st ->
    let last = st.fresh_var_idx in
    ST.put { st with fresh_var_idx = last + 1 } >>= fun () ->
    ST.return ("x" ^ (Int.to_string last))

  let add_symbolic_pair for_var fresh : unit ST.t =
    ST.update @@ fun st ->
    let alist' = (for_var, fresh) :: st.symbolic_var_map in
    { st with symbolic_var_map = alist' } 

  let new_symbolic for_var : string ST.t =
    make_fresh_symbolic >>= fun fresh ->
    add_symbolic_pair for_var fresh >>= fun () ->
    ST.return fresh

  let set_symbolic_val sym value : unit ST.t =
    ST.update @@ fun st ->
    let alist' = (sym, value) :: st.env in
    { st with env = alist' }             

  let fresh_bv_for_symbolic name bw : Expr.expr ST.t =
    ST.get () >>= fun st ->
    let sym = Z3.Symbol.mk_string ctxt name in
    let numeral = Bitv.mk_const ctxt sym bw in
    set_symbolic_val name numeral >>= fun () ->
    ST.return numeral

  let has_value_in_env symname : bool ST.t =
    let equal = String.equal in
    ST.gets @@ fun st ->
    List.Assoc.mem st.env ~equal symname

  let get_value_in_env_exn symname : Expr.expr ST.t =
    let equal = String.equal in
    ST.gets @@ fun st ->
    List.Assoc.find_exn st.env ~equal symname

  let get_value_in_env symname : Expr.expr option ST.t =
    let equal = String.equal in
    ST.gets @@ fun st ->
    List.Assoc.find st.env ~equal symname

  let has_symbolic_name varname : bool ST.t =
    let equal = String.equal in
    ST.gets @@ fun st ->
    List.Assoc.mem st.symbolic_var_map ~equal varname

  let get_symbolic_name_exn varname : string ST.t =
    let equal = String.equal in
    ST.gets @@ fun st ->
    List.Assoc.find_exn st.symbolic_var_map ~equal varname

  let set_was_load b : unit ST.t =
    ST.update @@ fun st -> { st with was_load = b }

  let was_load : bool ST.t =
    ST.gets @@ fun st -> st.was_load

  let set_last_loaded_symname symname : unit ST.t =
    was_load >>= fun was_load ->
    if was_load
    then
      ST.update @@ fun st ->
                   { st with last_loaded_symname = Some symname;
                             was_load = false }
    else
      ST.return ()

  let get_last_loaded_symname : string option ST.t =
    ST.gets @@ fun st -> st.last_loaded_symname

  let push_constraint ctr : unit ST.t =
    ST.update @@ fun st ->
    let cs' = st.constraints in
    { st with constraints = ctr :: cs' }

  let push_def_constraint lhs_symname rhs_symvalue : unit ST.t =
    get_value_in_env lhs_symname >>= fun lhs_symvalue ->
    (match lhs_symvalue with
      | Some lhs_symvalue -> ST.return lhs_symvalue
      | None ->
         let bw = Bitv.get_size (Expr.get_sort rhs_symvalue) in
         fresh_bv_for_symbolic lhs_symname bw
    ) >>= fun lhs_symvalue ->
    let eq_ctr = Bool.mk_eq ctxt lhs_symvalue rhs_symvalue in
    push_constraint eq_ctr

  let add_neq_constraint left right : unit ST.t =
    let neq = Bool.mk_not ctxt @@ Bool.mk_eq ctxt left right in
    push_constraint neq

  module SilentStoreChecks = struct
    let on_fail = fun st -> { st with failed_ss = true }
    
    let fail_ss = ST.update on_fail
  end

  module CompSimpChecks = struct
    let fail_cs_left : unit ST.t =
      ST.update @@ fun st -> { st with failed_cs_left = true }

    let fail_cs_right : unit ST.t =
      ST.update @@ fun st -> { st with failed_cs_right = true }

    let add_iden = fun width -> word @@ Word.of_int ~width 0
    
    let mul_iden = fun width -> word @@ Word.of_int ~width 1
    
    let mul_zero = fun width -> word @@ Word.of_int ~width 0

    let and_iden = fun width -> word @@ Word.lnot @@ Word.zero width

    let adds lw = [add_iden lw]

    let muls lw = [mul_zero lw; mul_iden lw]

    let ands lw = [add_iden lw; and_iden lw]

    let none = []

    let compound_checks (x, nots) : unit ST.t =
      List.fold nots ~init:(ST.return ()) ~f:(fun st shldnt ->
          st >>= fun () -> 
          add_neq_constraint x shldnt)

    (* what a great return type *)
    let check_binop (op : Bil.binop) left right : (unit ST.t * unit ST.t) ST.t =
      let lw = Bitv.get_size (Expr.get_sort left) in
      let rw = Bitv.get_size (Expr.get_sort right) in
      let no_checks = (left, []), (right, []) in
      let left_nots, right_nots = match op with
        | Bil.PLUS ->
           (left, adds lw), (right, adds rw)
        | Bil.MINUS ->
           (left, none), (right, adds rw)
        | Bil.TIMES ->
           (left, muls lw), (right, muls rw)
        | Bil.DIVIDE | Bil.SDIVIDE ->
           (left, [mul_zero lw]), (right, [mul_iden rw])
        | Bil.MOD  | Bil.SMOD ->
           (left, none), (right, none)
        | Bil.LSHIFT | Bil.RSHIFT -> 
           (left, muls lw), (right, muls rw)
        | Bil.ARSHIFT ->
           (left, ands lw), (right, [mul_zero rw])
        | Bil.AND ->
           (left, ands lw), (right, ands rw)
        | Bil.OR ->
           (left, ands lw), (right, ands rw) 
        | Bil.XOR ->
           (left, [mul_zero lw]), (right, [mul_zero rw])
        | Bil.EQ -> no_checks
        | Bil.NEQ -> no_checks
        | Bil.LT -> no_checks
        | Bil.LE -> no_checks
        | Bil.SLT -> no_checks
        | Bil.SLE -> no_checks
      in
      ST.return (compound_checks left_nots, compound_checks right_nots)

    let on_left_fail = fun st -> { st with failed_cs_left = true }

    let on_right_fail = fun st -> { st with failed_cs_right = true }
  end

  (* checkers use negative constraints -> this value should not
     be equal to this other value. a status of SAT then means
     a violation of a safety condition, UNSAT is ok, UNKNOWN means
     an error somewhere. 

     returns true = safe
             false = unsafe *)
  let check_now onfail : bool ST.t =
    ST.get () >>= fun st ->
    let status = Solver.check solver st.constraints in
    match status with
    | Solver.UNSATISFIABLE -> ST.return true
    | Solver.UNKNOWN ->
       failwith @@
         Format.sprintf "in Symbolic.Executor.check_now, error checking constraints for solver: %s : %s"
           (Solver.to_string solver)
           (Solver.get_reason_unknown solver)
    | Solver.SATISFIABLE ->
       ST.update onfail >>= fun () ->
       ST.return false

  let rec eval_exp (exp : Bil.exp) : Expr.expr option ST.t =
    match exp with
    | Bil.Load (_, idx, en, sz) ->
       set_was_load true >>= fun () ->
       let symname = Common.exp_to_string exp in
       let sz = Common.int_of_sz sz in
       fresh_bv_for_symbolic symname sz >>= fun value ->
       ST.return @@ Some value
    | Bil.Store (mem, idx, v, en, sz) ->
       ST.get () >>= fun st ->
       let mockload = Bil.Load (mem, idx, en, sz) in
       let memcellsymname = Common.exp_to_string mockload in
       eval_exp v >>= fun mcv ->
       let curval = match mcv with
         | Some curval -> curval
         | None -> failwith "In symex, unsupported data in store"
       in
       (if st.do_check && st.do_ss
       then
         has_value_in_env memcellsymname >>= fun valpresent ->
         if valpresent
         then
           get_value_in_env_exn memcellsymname >>= fun symval ->
           ST.get () >>= fun st_wo_constraints ->
           add_neq_constraint curval symval >>= fun () ->
           check_now SilentStoreChecks.on_fail >>= fun _store_safe ->
           (* ST.put st_wo_constraints >>= fun () -> *)
           ST.return None
         else
           SilentStoreChecks.fail_ss >>= fun () ->
           ST.return None
       else
         ST.return None) >>= fun res ->
       (* do a store now to syntactic mem cell *)
       push_def_constraint memcellsymname curval >>= fun () ->
       ST.return res
    | Bil.BinOp (op, x, y) ->
       eval_exp x >>= fun ml ->
       eval_exp y >>= fun mr ->
       begin
         match ml, mr with
         | Some l, Some r ->
            ST.gets (fun st -> st.do_check, st.do_cs) >>= fun (do_check, do_cs) ->
            begin
              if do_check && do_cs
              then
                let () = printf "[compsimp] Doing checks on exp: %a\n%!"
                           Exp.ppo exp in
                CompSimpChecks.check_binop op l r >>= fun (do_left_checks, do_right_checks) ->
                ST.get () >>= fun ini_st ->
                do_left_checks >>= fun () ->
                check_now CompSimpChecks.on_left_fail >>= fun _left_safe ->
                ST.gets (fun st -> st.failed_cs_left) >>= fun failed_cs_left ->
                ST.put ini_st >>= fun () ->
                do_right_checks >>= fun () ->
                check_now CompSimpChecks.on_right_fail >>= fun _right_safe ->
                ST.gets (fun st -> st.failed_cs_right) >>= fun failed_cs_right ->
                ST.put ini_st >>= fun () ->
                ST.update @@ fun st ->
                { st with failed_cs_left; failed_cs_right }
              else
                ST.return ()
            end >>= fun () ->
            ST.return @@ Some (binop op l r)
         | _, _ ->
            failwith "In Symbolic.Executor, unsupported ops of binop"
       end
    | Bil.UnOp (op, x) ->
       eval_exp x >>= fun mx ->
       begin
         match mx with
         | Some x -> ST.return @@ Some (unop op x)
         | None ->
            failwith "In Symbolic.Executor, unsupported op of unop"
       end
    | Bil.Var v ->
       let varname = Var.name v in
       has_symbolic_name varname >>= fun namepresent ->
       let symname = if namepresent
                     then get_symbolic_name_exn varname
                     else new_symbolic varname in
       symname >>= fun symname ->
       has_value_in_env symname >>= fun valpresent ->
       let bw = match ABI.size_of_var_name varname with
         | Some bw -> bw
         | None -> 64 in
       let symval = if valpresent
                    then get_value_in_env_exn symname
                    else fresh_bv_for_symbolic symname bw in
       symval >>= fun symval ->
       ST.return @@ Some symval
    | Bil.Int w -> ST.return @@ Some (word w)
    | Bil.Cast (typ, sz, x) ->
       eval_exp x >>= fun mx ->
       begin
         match mx with
         | Some x -> ST.return @@ Some (cast typ sz x)
         | None ->
            failwith "In Symbolic.Executor, unsupported op of cast"
       end
    | Bil.Let (_, _, _) ->
       failwith "in Symbolic.Executor.eval_exp, no support for let"
    | Bil.Unknown (_, _) ->
       failwith "in Symbolic.Executor.eval_exp, no support for unk"
    | Bil.Ite (i, t, e) ->
       eval_exp i >>= fun mi ->
       eval_exp t >>= fun mt ->
       eval_exp e >>= fun me ->
       begin
         match mi, mt, me with
         | Some i, Some t, Some e -> ST.return @@ Some (ite i t e)
         | _, _, _ ->
            failwith "in Symbolic.Executor.eval_exp, unsupported op of ite"
       end
    | Bil.Extract (hi, lo, x) ->
       eval_exp x >>= fun mx ->
       begin
         match mx with
         | Some x -> ST.return @@ Some (extract hi lo x)
         | None ->
            failwith "in Symbolic.Executor.eval_exp, unsupported op of extract"
       end
    | Bil.Concat (x, y) ->
       eval_exp x >>= fun ml ->
       eval_exp y >>= fun mr ->
       begin
         match ml, mr with
         | Some l, Some r ->
            ST.return @@ Some (concat l r)
         | _, _ ->
            failwith "In Symbolic.Executor, unsupported ops of concat"
       end

  let eval_def dt : unit ST.t =
    ST.return (Term.tid dt) >>= fun tid ->
    ST.update (fun st -> 
    { st with do_check = Tid.equal tid st.target_tid }) >>= fun () ->
    let () = printf "In symbolic.eval_def, evalling def term: %a\n%!"
               Tid.ppo tid in
    let rhs = Def.rhs dt in
    ST.gets (fun st ->
    printf "done evalling rhs of tid %a doing check? %B\n%!"
      Tid.ppo tid st.do_check) >>= fun () ->
    eval_exp rhs >>= fun mr ->
    match mr with
    | Some result ->
       let lhs = Def.lhs dt in
       let varname = Var.name lhs in
       new_symbolic varname >>= fun symname ->
       push_def_constraint symname result >>= fun () ->
       set_last_loaded_symname symname >>= fun () ->
       ST.update (fun st -> { st with do_check = false }) >>= fun () ->
       set_symbolic_val symname result
    | None ->
       ST.update @@ fun st -> { st with do_check = false }

  let eval_def_list defs : unit ST.t =
    let rec loop defs : unit ST.t =
      ST.get () >>= fun st ->
      if not st.failed_ss && not st.failed_cs_left && not st.failed_cs_right
      then
        match defs with
        | d :: defs' ->
           eval_def d >>= fun () ->
           loop defs'
        | [] -> ST.return ()
      else ST.return ()
    in
    let () = Solver.reset solver in (* clear old def constraints *)
    (* let init = ST.return () in *)
    let () = printf "in symbolic.eval_def_list: defs are\n%!";
             List.iter defs ~f:(printf "%a\n%!" Def.ppo) in
    loop defs
    (* List.fold defs ~init ~f:(fun st dt -> *)
    (*     ST.get () >>= fun st -> *)
    (*     if not st.failed_ss && not st.failed_cs_left && not st.failed_cs_right *)
    (*     then *)
    (*       eval_def dt *)
    (*     else *)
    (*       ST.return ()) *)

  let eval_blk blk : unit ST.t =
    let defs = Term.enum def_t blk |> Sequence.to_list in
    eval_def_list defs

  let run = ST.run
end
