open Core_kernel
open Bap.Std
open Graphlib.Std
open Common
open Abstract_memory

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

module Cfg = Bap.Std.Graphs.Cfg
module IrCfg = Bap.Std.Graphs.Ir

let print_iml iml : unit =
  Format.printf
        "%a\n%!"
        Sexp.pp
        (Map_lattice.Interval.M.sexp_of_t Wrapping_interval.sexp_of_t iml)

let print_sol sol : unit =
  Solution.enum sol |>
    Sequence.iter ~f:(fun (n, iml) ->
        Format.printf "Node (%a): \n%!" Graphs.Ir.Node.pp n;
        print_iml iml)

let insns_of_node n = Blk.elts @@ Graphs.Ir.Node.label n
let first_insn_of_blk b =
  let insns = Blk.elts b in
  match Seq.hd insns with
  | Some i -> i
  | None -> failwith "In first_insn_of_blk, couldn't find first insn"

let get_ret_insn_tid sub_nodes =
  let num = Seq.length sub_nodes in
  let last_node = Seq.nth_exn sub_nodes (num - 1) in
  let insns = insns_of_node last_node in
  let num_insns = Seq.length insns in
  let res =
    Seq.fold insns ~init:(None, 1) ~f:(fun (last, idx) insn ->
        let next_idx = idx + 1 in
        match idx, insn with
        | n, `Jmp j when n = num_insns -> Some (Term.tid j), next_idx
        | n, _ when n = num_insns -> failwith "Jmp/Ret was not last insn in sub"
        | _, _ -> None, next_idx)
  in
  match res with
  | Some tid, _ -> tid
  | _, _ -> failwith "Error finding last insn in sub"

let sub_of_tid tid proj : sub Term.t =
  let prog = Project.program proj in
  let sub = Term.find sub_t prog tid in
  match sub with
  | Some sub -> sub
  | None -> failwith "Didn't find sub with that tid in the program"

let last_insn_of_sub sub : Blk.elt =
  let irg = Sub.to_cfg sub in
  let rev_nodes = Graphlib.postorder_traverse (module Graphs.Ir) irg in
  let last_node = match Seq.hd rev_nodes with
    | Some n -> n
    | None ->
       begin
         let sub_name = Sub.name sub in
         let err_s = sprintf "Couldn't get last node of sub %s in last_insns_of_sub" sub_name in
         failwith err_s
       end
  in
  let last_node_insns = insns_of_node last_node in
  let num_insns = Seq.length last_node_insns in
  Seq.nth_exn last_node_insns (num_insns - 1)

let sub_to_insn_graph sub img ctxt proj =
  let print_elt = function
      | `Def d -> Format.printf "Def: %s\n%!" @@ Def.to_string d
      | `Phi p -> Format.printf "Phi: %s\n%!" @@ Phi.to_string p
      | `Jmp j -> Format.printf "Jmp: %s\n%!" @@ Jmp.to_string j
  in

  (* construct edges for converting from CFG with basic-block nodes
     to a CFG with insn nodes *)
  let edges, tidmap = Edge_builder.run_one sub proj in
  let () = printf "edges are:\n" in
  let () = List.iter edges ~f:Edge_builder.print_edge in

  (* run the liveness analysis *)
  let liveness = Live_variables.Analysis.run sub in
  
  (* CFG *)
  let module G = Graphlib.Make(Tid)(Bool) in
  let cfg = Graphlib.create (module G) ~edges () in

  (* AbsInt *)
  let module ProdIntvlxTaint = DomainProduct(Wrapping_interval)(Checker_taint.Analysis) in
  let module WithTypes = DomainProduct(ProdIntvlxTaint)(Type_domain) in
  let module FinalDomain = DomainProduct(WithTypes)(Bases_domain) in
  
  let module E = Abstract_memory.Make(FinalDomain) in
  let module R = Abstract_memory.Region in
  let module Rt = Abstract_memory.Region.Set in
  let module Vt = struct type t = Common.cell_t end in
  let module AbsInt = AbstractInterpreter(FinalDomain)(R)(Rt)(Vt)(E) in

  (* set up initial solution *)
  let empty = E.empty in
  let stack_addr = 0x7fff_fff0 in
  
  let free_vars = Sub.free_vars sub in
  let freenames = Set.to_list free_vars |> List.map ~f:Var.name in
  
  let args = Term.enum arg_t sub in
  let argnames = Seq.map args ~f:(fun a -> Arg.var a |> T.Var.name) |> Seq.to_list in
  let x64_args = ["RDI"; "RSI"; "RDX"; "RCX"; "R8"; "R9"] in

  (* e.g., filter out bap's 'mem' var *)
  let true_args = List.filter freenames
                    ~f:(fun name -> List.mem x64_args name ~equal:String.equal)
                  |> List.append argnames
  in

  let with_rsp_rbp_set = E.set_rbp stack_addr @@ E.set_rsp stack_addr empty in
  let with_img_set = E.set_img with_rsp_rbp_set img in

  let initial_mem = List.fold true_args ~init:with_img_set
                      ~f:(fun mem argname ->
                        E.set argname FinalDomain.top mem)
  in
  let () = Format.printf "Initial memory+env is: %!" in
  let () = E.pp initial_mem in
  let () = Format.printf "\n%!" in

  let first_node = match Seq.hd (Graphlib.reverse_postorder_traverse (module G) cfg) with
    | Some n -> n
    | None -> failwith "in cfg building init sol, couldn't get first node"
  in
  let () = printf "first node is %s\n" (Tid.to_string first_node) in

  let with_args = G.Node.Map.set G.Node.Map.empty ~key:first_node ~data:initial_mem in
  let init_sol = Solution.create with_args empty in

  let analysis_results = Graphlib.fixpoint
                           (module G)
                           cfg 
                           ~step:E.widen_with_step
                           ~init:init_sol
                           ~equal:E.equal
                           ~merge:E.merge
                           ~f:(fun tid ->
                             let elt = match Tid_map.find tidmap tid with
                               | Some elt -> elt
                               | None ->
                                  let tid_s = Tid.to_string tid in
                                  let err_s = sprintf "in calculating analysis_results, couldn't find tid %s in tidmap" tid_s in
                                  failwith err_s
                             in
                             AbsInt.denote_elt elt)
  in
  let () = printf "abstract interpretation finished\n%!";
           printf "starting comp simp checking\n%!" in

  (* Build up checker infra and run the checkers
   * This next part is an abomination of Ocaml code
   *)
  let analyze_edge (module Chkr : Checker.S with type env = E.t) (e : Edge_builder.edge) : Chkr.warns =
    let from_tid = Edge_builder.from_ e in
    let to_tid = Edge_builder.to_ e in
    let in_state = Solution.get analysis_results to_tid in
    let insn =
      match Tid_map.find tidmap to_tid with
      | Some elt -> elt
      | None ->
         let tid_str = Tid.to_string to_tid in
         failwith @@
           sprintf "In running checker %s, couldn't find tid %s" Chkr.name tid_str
    in
    let () = printf "checking edge (%a, %a)\n%!" Tid.ppo from_tid Tid.ppo to_tid in
    Chkr.check_elt insn liveness in_state
  in

  let run_checker (module Chkr : Checker.S with type env = E.t) (es : Edge_builder.edges) : Chkr.warns =
    List.fold edges
      ~init:Alert.Set.empty
      ~f:(fun alerts edge ->
        let alerts' = analyze_edge (module Chkr) edge in
        Alert.Set.union alerts alerts')
  in
  
  (* comp simp checking *)
  let module CSChecker : Checker.S with type env = E.t = struct
      include Comp_simp.Checker(FinalDomain)
      type env = E.t
    end in
  let module SSChecker : Checker.S with type env = E.t = struct
      include Silent_stores.Checker(FinalDomain)
      type env = E.t
    end in
  let comp_simp_alerts = run_checker (module CSChecker) edges in 
  let () = Alert.print_alerts comp_simp_alerts in
  let () = printf "Done with comp simp checking\n%!" in 
  let silent_store_alerts = run_checker (module SSChecker) edges in
  let () = Alert.print_alerts silent_store_alerts in
  let () = printf "Done with silent stores checking\n%!" in

  let runner = Callees.run () in
  let () =
    match KB.run Callees.cls runner (Toplevel.current ()) with
    | Error e -> Format.eprintf "Error: %a\n%!" KB.Conflict.pp e
    | Ok (v, _) -> Format.printf "named labels are %a\n%!" KB.Value.pp v
  in

  List.iter edges ~f:(fun (f, t, _is_interproc) ->
      let from_str = Tid.to_string f in
      let to_str = Tid.to_string t in
      Format.printf "(%s,%s)\n%!" from_str to_str);
      
  Tid_map.iteri tidmap
    ~f:(fun ~key ~data ->
      Format.printf "TID(%s) :: %!" (Tid.to_string key);
      print_elt data)

let iter_insns sub : unit =
  let irg = Sub.to_cfg sub in
  let free_vars = Sub.free_vars sub in
  let () = Var.Set.iter free_vars ~f:(fun v -> Format.printf "Free var: %s\n%!" (Var.name v)) in
  
  let nodes = Graphlib.reverse_postorder_traverse (module Graphs.Ir) irg in
  
  let print_sub_defs graphnode =
    let bb = Graphs.Ir.Node.label graphnode in
    let insns = Blk.elts bb in
    let print_insn = function
      | `Def d -> Format.printf "iter_insns--Def: %s\n%!" @@ Def.to_string d
      | `Phi p -> Format.printf "iter_insns--Phi: %s\n%!" @@ Phi.to_string p
      | `Jmp j -> Format.printf "iter_insns--Jmp: %s\n%!" @@ Jmp.to_string j
    in
    Seq.iter insns ~f:print_insn
  in
  
  let () = Format.printf "nodes are:\n%!" in
  Seq.iter nodes ~f:print_sub_defs

let check_fn (name, block, cfg) img ctxt proj =
  (* let prog = Project.program proj in *)
  (* let callgraph = Program.to_graph prog in *)
  (* let filename = sprintf "%s.dot" name in *)
  (* let string_of_node n = *)
  (*   Graphs.Callgraph.Node.label n *)
  (*   |> Tid.to_string *)
  (*   |> String.chop_prefix_if_exists ~prefix:"%" *)
  (* in *)
  (* let () = Graphlib.to_dot (module Graphs.Callgraph) callgraph ~filename ~string_of_node in *)
  let sub = Sub.lift block cfg in
  let () = iter_insns sub in
  (* Run the checkers *)
  sub_to_insn_graph sub img ctxt proj;
