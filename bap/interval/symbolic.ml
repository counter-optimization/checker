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
               "timeout", "1000"
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
      do_cs_left : bool;
      do_cs_right : bool;
      profiling_data_path : string;
      failed_ss : bool;
      failed_cs_left : bool;
      failed_cs_right : bool;
      type_info : Type_determination.t
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

  let debug_print_sym_env st : unit =
    let expr_to_str e : string =
      sprintf "(%s, (sort: %s, bw: %s))"
        (Expr.to_string e)
        (Sort.to_string @@ Expr.get_sort e)
        (if Bitv.is_bv e
         then sprintf "%d" @@ Bitv.get_size @@ Expr.get_sort e
         else "")
    in
    printf "var map is:\n%!";
    List.iter st.symbolic_var_map ~f:(fun (varname, symname) ->
        printf "(%s, %s)\n%!" varname symname);
    printf "type info is:\n%!";
    Type_determination.print st.type_info;
    printf "env map is:\n%!";
    List.iter st.env ~f:(fun (symname, expr) ->
        printf "(%s, %s)\n%!" symname (expr_to_str expr))

  let def_terms_as_string : string ST.t =
    ST.gets @@ fun st ->
    List.map st.defs ~f:Def.to_string
    |> String.concat ~sep:"\n"
    (* String.con *)
    (* let rec loop dts sofar = *)
    (*   match dts with *)
    (*   | [] -> sofar *)
    (*   | x :: xs -> *)
    (*      let cur_s = Def.to_string x in *)
    (*      let sofar' = if String.is_empty sofar *)
    (*                   then cur_s *)
    (*                   else sofar ^ "\n" ^ cur_s *)
    (*      in *)
    (*      loop xs sofar' *)
    (* in *)
    (* loop st.defs "" *)

  let write_csv_profile_data csvrow : unit ST.t =
    ST.gets @@ fun st -> 
    let file_path = st.profiling_data_path in
    let binary = false in
    let append = true in
    Out_channel.with_file ~binary ~append
      file_path ~f:(fun out_ch ->
        let final_row = csvrow ^ "\n" in
        Out_channel.output_string out_ch final_row;
        Out_channel.flush out_ch)

  let init ?(do_ss : bool = false)
        ?(do_cs : bool = false)
        ?(do_cs_left = false)
        ?(do_cs_right = false)
        defs target_tid type_info
        profiling_data_path = {
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
      do_cs_left;
      do_cs_right;
      profiling_data_path;
      failed_ss = false;
      failed_cs_left = false;
      failed_cs_right = false;
      type_info
    }

  let coerce_shift op ctxt x y =
    let sx = Bitv.get_size (Expr.get_sort x)
    and sy = Bitv.get_size (Expr.get_sort y) in
    if sx = sy then op ctxt x y
    else
      let y = Bitv.mk_zero_ext ctxt (sx-sy) y in
      op ctxt x y

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
    | LT -> fun ctxt l r -> bit_of_bool @@ Bitv.mk_ult ctxt l r
    | LE -> fun ctxt l r -> bit_of_bool @@ Bitv.mk_ule ctxt l r
    | SLT -> fun ctxt l r -> bit_of_bool @@ Bitv.mk_slt ctxt l r
    | SLE -> fun ctxt l r -> bit_of_bool @@ Bitv.mk_sle ctxt l r
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

  let binop op x y = z3_of_binop op ctxt x y
  
  let unop op x = z3_of_unop op ctxt x

  let word x =
    (* let () = printf "in symbolic.word, word is %a, bitwidth is %d\n%!" *)
    (*            Word.ppo x (Word.bitwidth x) in *)
    let s = Bitv.mk_sort ctxt (Word.bitwidth x) in
    let x = Word.to_bitvec x in
    (* let () = printf "that word fits in int from bitvec? %B\n%!" @@ *)
    (*            Bitvec.fits_int x in *)
    if Bitvec.fits_int x
    then Expr.mk_numeral_int ctxt (Bitvec.to_int x) s
    else
      let x = Bitvec.to_bigint x in
      Expr.mk_numeral_string ctxt (Z.to_string x) s

  let extract hi lo x =
    (* let () = printf "in symbolic.extract: hi=%d, lo=%d, x=%s\n%!" hi lo @@ *)
    (*            Expr.to_string x in *)
    let xs = Bitv.get_size (Expr.get_sort x)
    and ns = hi-lo+1 in
    (* let () = printf "in symbolc.extract, x size is: %d\n%!" xs in *)
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
         let rhs_sort = Expr.get_sort rhs_symvalue in
         (* let () = printf "in pushing def constraint, rhs sort is %s\n%!" @@ *)
         (*            Z3.Sort.to_string rhs_sort in *)
         let bw = Bitv.get_size rhs_sort in
         fresh_bv_for_symbolic lhs_symname bw
    ) >>= fun lhs_symvalue ->
    let eq_ctr = Bool.mk_eq ctxt lhs_symvalue rhs_symvalue in
    push_constraint eq_ctr

  let add_neq_constraint left right : unit ST.t =
    let neq = Bool.mk_not ctxt @@ Bool.mk_eq ctxt left right in
    push_constraint neq

  let add_is_eq_constraint left right : unit ST.t =
    push_constraint @@ Bool.mk_eq ctxt left right

  let set_width width : int =
    match width with
    | 1 -> 1
    | 8 -> 8
    | 16 -> 16
    | 32 -> 32
    | 64 -> 64
    | 128 -> 128
    | 256 -> 256
    | _ ->
       let rec loop width allowed_widths : int =
         match allowed_widths with
         | [] -> failwith @@ sprintf "Couldn't get width for %d" width
         | x :: xs -> if width <= x
                      then x
                      else loop width xs
       in
       loop width [1; 8; 16; 32; 64; 128; 256]

  let set_free_vars : unit ST.t =
    ST.gets (fun st -> Type_determination.get_all_typed_vars st.type_info)
    >>= fun typed_vars ->
    List.fold typed_vars ~init:(ST.return ())
      ~f:(fun st varname ->
        st >>= fun () ->
        ST.gets (fun st -> st.type_info) >>= fun type_info ->
        let maybe_width = Type_determination.get_bitwidth varname type_info in
        let width = Option.value_exn maybe_width in
        new_symbolic varname >>= fun symname ->
        let allowed_width = match ABI.size_of_var_name varname with
          | Some size -> size
          | None -> set_width width
        in
        fresh_bv_for_symbolic symname allowed_width >>= fun symval ->
        set_symbolic_val symname symval)

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

    let compound_checks (x, badvals) : unit ST.t =
      (* List.fold nots ~init:(ST.return ()) ~f:(fun st shldnt -> *)
      (*     st >>= fun () -> add_neq_constraint x shldnt) *)
      let eq_constrs = List.map badvals ~f:(Bool.mk_eq ctxt x) in
      let any_eq_constr = Bool.mk_or ctxt eq_constrs in
      push_constraint any_eq_constr

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

  let get_solver_string constraints : string =
    let () = Solver.add solver constraints in
    let solver_state_string = Solver.to_string solver in
    let () = Solver.reset solver in
    solver_state_string

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
    | Solver.UNSATISFIABLE ->
       let reason = Solver.get_reason_unknown solver in
       let () = printf "UNSAT: %s\n%!" reason in
       ST.return true
    | Solver.UNKNOWN ->
       let reason = Solver.get_reason_unknown solver in
       if String.Caseless.equal reason "timeout"
       then
         let () = printf "[SilentStores] symbolic compilation timed out for tid: %a\n%!" Tid.ppo st.target_tid in
         ST.update onfail >>= fun () ->
         ST.return false
       else
         failwith @@
           Format.sprintf "in Symbolic.Executor.check_now, error checking constraints for solver: %s : %s"
             (Solver.to_string solver)
             reason
    | Solver.SATISFIABLE ->
       ST.update onfail >>= fun () ->
       ST.return false

  let rec supports_exp (exp : Bil.exp) : bool =
    match exp with
    | Bil.Load (_, idx_exp, _, _) ->
       supports_exp idx_exp
    | Bil.Store (_, idx, v, _, _) ->
       supports_exp idx && supports_exp v
    | Bil.BinOp (_, l, r) ->
       supports_exp l && supports_exp r
    | Bil.UnOp (_, l) ->
       supports_exp l
    | Bil.Var _ -> true
    | Bil.Int _ -> true
    | Bil.Cast (_, _, subexp) ->
       supports_exp subexp
    | Bil.Let (_, _, _) -> false
    | Bil.Unknown (_, (Imm n)) -> true
    | Bil.Unknown (_, _) -> false
    | Bil.Ite (cond, then_, else_) ->
       supports_exp cond &&
         supports_exp then_ &&
           supports_exp else_
    | Bil.Extract (_, _, subexp) -> 
       supports_exp subexp
    | Bil.Concat (l, r) ->
       supports_exp l && supports_exp r

  let rec eval_exp (exp : Bil.exp) : Expr.expr option ST.t =
    ST.get () >>= fun _ ->
    let () = printf "in symbolic eval_exp, evaluating exp %a\n%!"
               Exp.ppo exp
    in
    let res = match exp with
    | Bil.Load (_, idx, en, sz) ->
       eval_exp idx >>= fun _idx_val ->  
       set_was_load true >>= fun () ->
       let symname = Common.exp_to_string exp in
       let sz = Common.int_of_sz sz in
       fresh_bv_for_symbolic symname sz >>= fun value ->
       ST.return @@ Some value
    | Bil.Store (mem, idx, v, en, sz) ->
       ST.get () >>= fun st ->
       let mockload = Bil.Load (mem, idx, en, sz) in
       let memcellsymname = Common.exp_to_string mockload in
       eval_exp idx >>= fun idx_val -> 
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
           (* add_neq_constraint curval symval >>= fun () -> *)
           add_is_eq_constraint curval symval >>= fun () ->
           
           let start' = Time_ns.now () in
           check_now SilentStoreChecks.on_fail >>= fun _store_safe ->
           let end' = Time_ns.now () in
           let chk_time = Time_ns.diff end' start' |> Time_ns.Span.to_int_ns in

           def_terms_as_string >>= fun def_term_str ->
           ST.gets (fun st -> st.constraints) >>= fun constraints ->

           let silent_store_const_str = get_solver_string constraints in 
           let profiling_data_csv_row = sprintf ",1,%d,,,\"%s\",,\"%s\""
                                          chk_time
                                          silent_store_const_str
                                          def_term_str
           in
           write_csv_profile_data profiling_data_csv_row >>= fun () ->
           
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
                CompSimpChecks.check_binop op l r >>= fun (do_left_checks, do_right_checks) ->
                ST.get () >>= fun ini_st ->
                let do_left_checks = if ini_st.do_cs_left
                                     then do_left_checks
                                     else ST.return ()
                in
                let do_right_checks = if ini_st.do_cs_right
                                      then do_right_checks
                                      else ST.return ()
                in
                
                do_left_checks >>= fun () ->
                let start' = Time_ns.now () in
                check_now CompSimpChecks.on_left_fail >>= fun _ ->
                let end' = Time_ns.now () in
                let left_chk_time =
                  if ini_st.do_cs_left
                  then
                    Time_ns.Span.to_int_ns @@ Time_ns.diff end' start'
                  else
                    0
                in
                ST.get () >>= fun with_left_const ->
                let left_const_str = get_solver_string with_left_const.constraints in
                let failed_cs_left = with_left_const.failed_cs_left in
                ST.put ini_st >>= fun () ->
                
                do_right_checks >>= fun () ->
                let start' = Time_ns.now () in
                check_now CompSimpChecks.on_right_fail >>= fun _ ->
                let end' = Time_ns.now () in
                let right_chk_time =
                  if ini_st.do_cs_right
                  then
                    Time_ns.Span.to_int_ns @@ Time_ns.diff end' start'
                  else
                    0
                in
                (* for profiling *)
                ST.get () >>= fun with_right_const ->
                let right_const_str = get_solver_string with_right_const.constraints in
                let failed_cs_right = with_right_const.failed_cs_right in
                
                def_terms_as_string >>= fun def_term_str ->
                let profiling_data_csv_row = sprintf "1,,,%d,%d,\"%s\",\"%s\",\"%s\""
                                               left_chk_time
                                               right_chk_time
                                               (if ini_st.do_cs_left
                                                then left_const_str
                                                else "")
                                               (if ini_st.do_cs_right
                                                then right_const_str
                                                else "")
                                               def_term_str
                in
                write_csv_profile_data profiling_data_csv_row >>= fun () ->
                (* let () = printf "In symex checker, left check time is %d, right check time was %d\n%!" left_chk_time right_chk_time in *)
                ST.update @@ fun st ->
                { ini_st with failed_cs_left; failed_cs_right }
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
                     else new_symbolic varname
       in
       symname >>= fun symname ->
       has_value_in_env symname >>= fun valpresent ->
       begin
         if valpresent
         then get_value_in_env_exn symname
         else
           begin
             match ABI.size_of_var_name varname with
             | Some bw -> fresh_bv_for_symbolic symname bw
             | None ->
                let () = printf "Couldn't get bitwidth for var %s in symex compile of var" varname in
                fresh_bv_for_symbolic symname 64
           end
       end >>= fun symval ->
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
    | Bil.Unknown (_, (Imm n)) ->
       make_fresh_symbolic >>= fun symname ->
       fresh_bv_for_symbolic symname n >>= fun bv ->
       ST.return @@ Some bv
    | Bil.Unknown (_, _) ->
       failwith "in Symbolic.Executor.eval_exp, no support for generic unk"
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
    in
    ST.get () >>= fun _ ->
    (* let () = printf "in symbolic eval_exp, exiting exp %a\n%!" *)
    (*            Exp.ppo exp in *)
    res

  let eval_def dt : unit ST.t =
    ST.return (Term.tid dt) >>= fun tid ->
    ST.update (fun st -> 
    { st with do_check = Tid.equal tid st.target_tid }) >>= fun () ->
    let () = printf "In symbolic.eval_def, evalling def term (%a) %a\n%!"
               Tid.ppo tid
               Def.ppo dt
    in
    let rhs = Def.rhs dt in
    eval_exp rhs >>= fun mr ->
    (* ST.gets (fun st -> *)
    (* printf "done evalling rhs of tid %a doing check? %B\n%!" *)
    (*   Tid.ppo tid st.do_check; *)
    (* debug_print_sym_env st) >>= fun () -> *)
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
    let () = printf "in symbolic.eval_def_list: defs are\n%!";
             List.iter defs ~f:(printf "%a\n%!" Def.ppo) in
    set_free_vars >>= fun () ->
    ST.gets (fun st ->
        printf "done setting free vars\n%!";
        debug_print_sym_env st) >>= fun () ->
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
