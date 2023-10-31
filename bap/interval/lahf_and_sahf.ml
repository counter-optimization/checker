open Core_kernel
open Bap.Std
open Graphlib.Std

type t = Tidset.t

type state =
  | Inside
  | InsideMaybeSahf
  | Outside

let log_prefix = sprintf "%s.lahfsahf" Common.package
module L = struct
  include Dolog.Log
  let () = set_prefix log_prefix
end

let default = Tid.Set.empty

let tid_part_of_transform = Tidset.mem

let lahf_insn_var_names =
  ["RAX"; "SF"; "ZF"; "AF"; "PF"; "CF"]
  |> String.Set.of_list

let run_on_cfg (type g) (module G : Graph with type t = g and type node = Calling_context.t) g tidmap =
  let is_rax_load = function
    | `Def d ->
      begin match Def.rhs d with
      | Bil.Cast (HIGH, 8, Bil.Cast (LOW, 16, Bil.Var v)) ->
        String.Caseless.equal (Var.name v) "rax"
      | _ -> false end
    | _ -> false
  in
  let is_flag_store = function
    | `Def d ->
      Set.mem Common.ABI.flag_names (Var.name (Def.lhs d)) &&
      begin match Def.rhs d with
      | Bil.Extract (hi, lo, subexp) when hi = lo -> true
      | _ -> false end
    | _ -> false
  in
  let likely_lahf = function
    | `Def d ->
      if String.equal "RAX" (Var.name (Def.lhs d))
      then
        let rhs = Def.rhs d in
        let rhs_var_names = Var_name_collector.run rhs in
        String.Set.equal rhs_var_names lahf_insn_var_names
      else false
    | _ -> false
  in
  let tween_tids = ref Tid.Set.empty in
  let delay_tid : tid option ref = ref None in
  let interp_node cc (st : state) =
    let tid = Calling_context.to_insn_tid cc in
    let elt = match Tid_map.find tidmap tid with
      | Some elt -> elt
      | None ->
        L.error "Couldnt' find tid %a in lahf_sahf run_on_cfg"
          Tid.ppo tid;
        failwith "lahf_and_sahf.run_on_cfg error"
    in
    match st with
    | Inside when is_rax_load elt ->
      delay_tid := Some tid; InsideMaybeSahf
    | InsideMaybeSahf when is_flag_store elt ->
      delay_tid := None; Outside
    | InsideMaybeSahf when is_rax_load elt ->
      delay_tid := Some tid; InsideMaybeSahf
    | InsideMaybeSahf ->
      (if Option.is_some !delay_tid
       then tween_tids := Set.add !tween_tids (Option.value_exn !delay_tid));
      Inside
    | Inside ->
      tween_tids := Set.add !tween_tids tid;
      Inside
    | Outside when likely_lahf elt -> Inside
    | Outside -> Outside
  in
  let merge l r = match l, r with
    | Outside, Outside -> Outside
    | Inside, Inside -> Inside
    | InsideMaybeSahf, InsideMaybeSahf -> InsideMaybeSahf
    | Inside, _ -> Inside
    | _, Inside -> Inside
    | InsideMaybeSahf, _ -> InsideMaybeSahf
    | _, InsideMaybeSahf -> InsideMaybeSahf
  in
  let equal l r = match l, r with
    | Outside, Outside -> true
    | Inside, Inside -> true
    | InsideMaybeSahf, InsideMaybeSahf -> true
    | _, _ -> false
  in
  let step _visits _node = merge in
  let init_map = G.Node.Map.empty in
  let init_sol = Solution.create init_map Outside in
  let _res = Graphlib.fixpoint (module G) g
               ~step
               ~equal
               ~merge
               ~init:init_sol
               ~f:interp_node
  in
  !tween_tids
    
let print tweens =
  printf "Insns between lahf and sahf:\n%!";
  Tidset.iter tweens ~f:(L.debug "\t%a" Tid.ppo)
