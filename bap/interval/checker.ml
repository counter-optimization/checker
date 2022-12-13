open Core_kernel
open Bap.Std
open Graphlib.Std
open Common

module type Checker = sig
  type warns = Alert.Set.t
  type t = warns

  val check_elt : Blk.elt -> 'a -> 'b -> warns
  val name : string
end

let run (module C : Checker)
      (edges: Edge_builder.edges)
      (analysis_results : ('a, 'b) Solution.t)
      (liveness : Live_variables.t)
      (tidmap : 'c Tid_map.t)
    : C.warns =
  let analyze_edge (e : Edge_builder.edge) : C.warns =
    let from_tid = Edge_builder.from_ e in
    let to_tid = Edge_builder.to_ e in
    let in_state = Solution.get analysis_results from_tid in
    let insn = match Tid_map.find tidmap to_tid with
      | Some elt -> elt
      | None ->
         let tid_str = Tid.to_string to_tid in
         failwith @@
           sprintf "In running checker %s, couldn't find tid %s" C.name tid_str
    in
    C.check_elt insn liveness in_state
  in
  List.fold edges
    ~init:Alert.Set.empty
    ~f:(fun alerts edge ->
      let alerts' = analyze_edge edge in
      Alert.Set.union alerts alerts')
