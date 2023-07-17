open Core_kernel
open Common
open Bap.Std
open Graphlib.Std
open Monads.Std

type global_const_store = { addr : word; data : word }

module Libsodium = struct
  
  let toplevel_init_fn_name = "sodium_init"

  let which_implementation_idx_to_pick = 0

  module Analysis = struct
    open Or_error.Monad_infix

    (* module CalleesGetter = Callees.Getter(Wrapping_interval) *)
    
    module SubSet = Set.Make_using_comparator(Sub)

    module WordSet = Set.Make_using_comparator(Word)
    
    let analysis_name = "Global_function_pointers.Libsodium.Analysis"

    let sub_is_toplevel_init_fn sub =
      String.equal (Sub.name sub) toplevel_init_fn_name
    
    let get_toplevel_init_fn ctxt proj =
      let prog = Project.program proj in
      let subs = Term.enum sub_t prog in
      let toplevel = Seq.filter subs ~f:sub_is_toplevel_init_fn in
      if Seq.length toplevel = 1
      then Seq.hd toplevel
      else
        let () =
          printf "In %s.get_toplevel_init_fn, couldn't find sub with name %s"
            analysis_name toplevel_init_fn_name
        in
        None

    let sub_of_tid (sub_tid : tid) (prog : Program.t) : sub term option =
      Term.find sub_t prog sub_tid

    let get_direct_callees sub proj : SubSet.t =
      let is_direct_call (j : jmp term) sub : Common.CalleeRel.t option =
        let caller_tid = Term.tid sub in
        let callsite_tid = Term.tid j in
        match Jmp.kind j with
        | Call callee ->
           let target = Call.target callee in
           (match target with
            | Direct totid -> Some { caller = caller_tid ;
                                     callee = totid ;
                                     callsite = callsite_tid }
            | _ -> None)
        | _ -> None in
      let get_direct_callees sub proj : Common.CalleeRel.t list =
        let blks = Term.enum blk_t sub in
        Seq.map blks ~f:(fun b -> Term.enum jmp_t b |> Seq.to_list)
        |> Seq.to_list
        |> List.join
        |> List.fold ~init:[] ~f:(fun acc jt ->
                       match is_direct_call jt sub with
                       | None -> acc
                       | Some callee_rel -> callee_rel :: acc) in
      let prog = Project.program proj in
      let callee_rels = get_direct_callees sub proj in
      let callee_tids = List.map callee_rels ~f:(fun cr -> cr.callee) in
      List.fold callee_tids ~init:SubSet.empty
                ~f:(fun acc ct ->
                  match sub_of_tid ct prog with
                  | None -> acc
                  | Some sub -> SubSet.add acc sub)

    let get_all_direct_callees sub ctxt proj =
      let rec worklist_loop ~(processed : SubSet.t) ~(worklist : SubSet.t) : SubSet.t =
        if SubSet.is_empty worklist
        then processed
        else
          let picked = match SubSet.nth worklist 0 with
            | Some callee -> callee
            | None ->
               failwith "in Global_function_pointers.get_all_direct_callees, shouldn't happen" in
          let worklist' = SubSet.remove worklist picked in
          if SubSet.mem processed picked
          then worklist_loop ~processed ~worklist:worklist'
          else
            let those_callees = get_direct_callees sub proj in
            let worklist'' = SubSet.union those_callees worklist' in
            let processed' = SubSet.add processed picked in
            worklist_loop ~processed:processed' ~worklist:worklist'' in
      let init_worklist = SubSet.singleton sub in
      let init_processed = SubSet.empty in
      worklist_loop ~processed:init_processed ~worklist:init_worklist

    let get_all_initializers ctxt proj =
      Option.bind (get_toplevel_init_fn ctxt proj) ~f:(fun toplevel_init_fn ->
      let all_initializers = get_all_direct_callees toplevel_init_fn ctxt proj in
      Some all_initializers)

    let get_global_store_addr (elt : Blk.elt) : addr option =
      match elt with
      | `Def d ->
         let rhs = Def.rhs d in
         (match rhs with
          | Bil.Load (_, _, _, _) -> None
          (* if it's just setting global vars, then ignore it *)
          | Bil.Store (_, (Bil.Int addr), (Bil.Int to_store), _, _) ->
             if Word.(to_store < of_int ~width:64 4000)
             then None
             else Some addr
          | Bil.Store (_, (Bil.Int addr), _to_store, _, _) -> Some addr
          | Bil.Store (_, _, _, _, _) -> None
          | Bil.BinOp (_, _, _) -> None
          | Bil.UnOp (_, _) -> None
          | Bil.Var _ -> None
          | Bil.Int _ -> None
          | Bil.Cast (_, _, _) -> None
          | Bil.Let (_, _, _) -> None
          | Bil.Unknown (_, _) -> None
          | Bil.Ite (_, _, _) -> None
          | Bil.Extract (_, _, _) -> None
          | Bil.Concat (_, _) -> None)
      | `Phi _ -> None
      | `Jmp _ -> None

    let get_def_of_const (elt : Blk.elt) : addr option =
      match elt with
      | `Def d ->
         let rhs = Def.rhs d in
         (match rhs with
          | Bil.Load (_, _, _, _) -> None
          | Bil.Store (_, _, _, _, _) -> None
          | Bil.BinOp (_, _, _) -> None
          | Bil.UnOp (_, _) -> None
          | Bil.Var _ -> None
          | Bil.Int constdata -> Some constdata
          | Bil.Cast (_, _, _) -> None
          | Bil.Let (_, _, _) -> None
          | Bil.Unknown (_, _) -> None
          | Bil.Ite (_, _, _) -> None
          | Bil.Extract (_, _, _) -> None
          | Bil.Concat (_, _) -> None)
      | `Phi _ -> None
      | `Jmp _ -> None

    let rec get_store_info ~(allelts : Blk.elt Seq.t) ~(prevelt : Blk.elt option)
                           ~(stores : global_const_store list)
            : global_const_store list =
      let curelt = Seq.hd allelts in
      let restelts = Seq.tl allelts in
      let nextelts = if Option.is_some restelts
                     then Option.value_exn restelts
                     else Seq.empty in
      match curelt with
      | None -> stores (* no more to process *)
      | Some thiselt ->         (* process current head of elts *)
         (match prevelt with
          | None ->             (* if there wasn't a prev, then set cur to prev *)
             get_store_info ~allelts:nextelts ~prevelt:curelt ~stores
          | Some prevelt -> (* there was a prev elt, check if it was a def of const *)
             let def_of_const = get_def_of_const prevelt in
             let store_of_global = get_global_store_addr thiselt in
             match def_of_const, store_of_global with
             | Some storedata, Some storeaddr ->
                let stores' = { addr = storeaddr; data = storedata } :: stores in
                get_store_info ~allelts:nextelts ~prevelt:curelt ~stores:stores'
             | _, _ ->
                get_store_info ~allelts:nextelts ~prevelt:curelt ~stores)

    let global_stores_of_sub sub : addr list =
      let elts = Term.enum blk_t sub
                 |> Seq.map ~f:Blk.elts
                 |> Seq.join in
      Seq.fold elts ~init:[] ~f:(fun acc elt ->
                 match get_global_store_addr elt with
                 | None -> acc
                 | Some addr -> addr :: acc)

    let global_store_info_of_sub sub : global_const_store list =
      let elts = Term.enum blk_t sub
                 |> Seq.map ~f:Blk.elts
                 |> Seq.join in
      get_store_info ~allelts:elts ~prevelt:None ~stores:[]

    (* let get_global_storing_subs ctxt proj : sub term list = *)
    (*   let all_initializers = get_all_initializers ctxt proj in *)
    (*   SubSet.filter all_initializers ~f:(fun sub -> *)
    (*                   let global_stores = global_stores_of_sub sub in *)
    (*                   let () = printf "global stores of %s are:\n%!" (Sub.name sub); *)
    (*                            List.iter global_stores ~f:(fun addr -> *)
    (*                                        Format.printf "%a\n%!" Word.pp addr) in *)
    (*                   not (List.is_empty global_stores)) *)
    (*   |> SubSet.to_list *)

    let get_all_init_fn_ptr_data ctxt proj : global_const_store list =
      match get_all_initializers ctxt proj with
      | None -> []
      | Some all_initializers ->
         let bss_stores = SubSet.fold all_initializers ~init:[] ~f:(fun acc sub ->
                              let store_info = global_store_info_of_sub sub in
                              List.append store_info acc) in
         List.fold bss_stores ~init:(WordSet.empty, [])
           ~f:(fun (added_addrs, added_stores) { addr; data } ->
             if WordSet.mem added_addrs addr
             then (added_addrs, added_stores)
             else (WordSet.add added_addrs addr, { addr; data } :: added_stores))
         |> snd
  end
end
