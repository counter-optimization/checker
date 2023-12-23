open Core_kernel
open Bap.Std
open Monads.Std

module BapG = Graphlib.Std
module OcamlG = Graph

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

let log_prefix = sprintf "%s.uc_fixpoint" Common.package
module L = struct
  include Dolog.Log
  let () = set_prefix log_prefix
end

module WidenSet = struct
  module G = Uc_graph_builder.UcOcamlG.T
  module WTO = OcamlG.WeakTopological.Make(G)

  let get_wto g start_node =
    WTO.recursive_scc g start_node

  let compute_wto g start_node =
    let wto = get_wto g start_node in
    let init = Tid.Set.empty in
    let aggregate = fun widenset -> function
      | OcamlG.WeakTopological.Component (hd, rst) ->
        Tid.Set.add widenset hd
      | _ -> widenset
    in
    OcamlG.WeakTopological.fold_left aggregate init wto
end

module SingleShotRoundRobinNarrow (Dom : Abstract.NumericDomain) : sig
  val compute : 'a 'd. (module BapG.Graph with type t = 'a and type node = Calling_context.t) -> 'a -> initsol:(Calling_context.t, 'd) BapG.Solution.t -> tidmap:Blk.elt Tid.Map.t -> start:Calling_context.t -> f:(Calling_context.t -> 'd -> 'd) -> merge:('d -> 'd -> 'd) -> default:'d -> d_equal:('d -> 'd -> bool) -> (Calling_context.t, 'd) BapG.Solution.t
end = struct
  type 'd compute_result = Done of 'd Calling_context.Map.t
                      | Continue of (Int.Set.t * 'd Calling_context.Map.t)

  let continue x = Continue x 
  
  let compute (type a d) (module G : BapG.Graph with type t = a and type node = Calling_context.t) g ~initsol ~tidmap ~start ~f ~merge ~default ~d_equal =
    let nodes =
      BapG.Graphlib.reverse_postorder_traverse (module G) ~start g |>
      Sequence.to_array in
    let rnodes =
      Array.foldi nodes ~init:G.Node.Map.empty ~f:(fun i rnodes n ->
        Map.set rnodes ~key:n ~data:i) in
    let succs = Array.map nodes ~f:(fun n ->
      let succs = G.Node.succs in
      succs n g |> Sequence.fold ~init:Int.Set.empty ~f:(fun ns n ->
        match Map.find rnodes n with
        | None -> ns
        | Some i -> Set.add ns i)) in
    let visited = Array.init (Array.length nodes) ~f:(fun _ -> false) in
    let visit n = Array.set visited n true in
    let seen n = Array.get visited n in
    let get approx n = match Calling_context.Map.find approx n with
      | Some v -> v
      | None -> default
    in
    let step works approx : d compute_result =
      match Int.Set.min_elt works with
      | None -> Done approx
      | Some next when seen next ->
        Continue (Int.Set.remove works next, approx)
      | Some next -> 
        let works = Int.Set.remove works next in
        visit next;
        let cc = nodes.(next) in
        let cur_ap = get approx cc in
        let out = f cc cur_ap in
        succs.(next)
        |> Set.fold ~init:(works, approx)
             ~f:(fun (works, approx) n ->
               let cc = nodes.(n) in
               let ap = get approx cc in
               let ap' = merge ap out in
               if d_equal ap ap'
               then (works, approx)
               else
                 let key = nodes.(n) in
                 (Int.Set.add works n,
                  Calling_context.Map.set approx ~key ~data:ap'))
        |> continue
    in
    let rec loop works approx =
      match step works approx with
      | Done approx -> approx
      | Continue (works, approx) -> loop works approx
    in
    let worklist = List.init (Array.length nodes) ~f:Fn.id
                   |> Int.Set.of_list in
    let initsol = BapG.Solution.enum initsol
                  |> Calling_context.Map.of_sequence_exn in
    loop worklist initsol
    |> (fun m -> BapG.Solution.create m default)
end
