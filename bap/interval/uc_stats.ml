open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_knowledge.Knowledge

let src = Uc_log.create_src "uc-stats"

let int_to_dom = KB.Domain.total
                   ~inspect:Int.sexp_of_t
                   ~join:(fun x y ->
                     Ok (Int.max x y))
                   ~empty:0
                   ~order:Int.compare
                   "totally-ordered-int63s"

(** convenience *)
let public = true
let package = Common.package

(** classes *)
type checker_stats

let checker_stat_cls : (checker_stats, unit) KB.cls =
  KB.Class.declare "checker-stats" ()
    ~package
    ~public
    ~desc:"Class for holding checker stats"

(** class slots *)
type stat_type = (checker_stats, int) KB.slot
let total =
  KB.Class.property
    checker_stat_cls
    "Total number of instructions considered"
    ~package
    ~public
    int_to_dom

let bv_pruned =
  KB.Class.property
    checker_stat_cls
    "Instructions pruned by bv"
    ~package
    ~public
    int_to_dom

let taint_pruned =
  KB.Class.property
    checker_stat_cls
    "Instructions pruned by taint"
    ~package
    ~public
    int_to_dom

let interval_pruned =
  KB.Class.property
    checker_stat_cls
    "Instructions pruned by interval"
    ~package
    ~public
    int_to_dom

let interproc_pruned =
  KB.Class.property
    checker_stat_cls
    "Instructions pruned by interprocedural-ness"
    ~package
    ~public
    int_to_dom

let symex_pruned =
  KB.Class.property
    checker_stat_cls
    "Instructions pruned by symbolic execution"
    ~package
    ~public
    int_to_dom

let unsupported_pruned =
  KB.Class.property
    checker_stat_cls
    "Instructions pruned due to being unsupported"
    ~package
    ~public
    int_to_dom

(** KB symbols for interning *)
type stat_category = string
let dmp_stats = "dmp-stats"
let cs_stats = "cs-stats"
let ss_stats = "ss-stats"

(** stats updating *)
open KB.Syntax

type t = (checker_stats, unit) KB.cls KB.value

type count = int
       
let get_obj stat =
  KB.Symbol.intern ~public ~package stat checker_stat_cls

let get statcat =
  let obj = get_obj statcat in
  let kb = Toplevel.current () in
  match KB.run checker_stat_cls obj kb with
  | Ok (stat, _) -> stat
  | Error e -> failwith @@ KB.Conflict.to_string e

let stat stats stattype =
  KB.Value.get stattype stats

let incr statcat (stattype : stat_type) =
  Toplevel.exec begin
    let* stats = get_obj statcat in
    let* count = stats-->stattype in
    KB.provide stattype stats (count + 1)
  end
    
let to_json_string stats =
  let s n v = sprintf "\"%s\" : \"%d\"" n v in
  let print_order = ["total_considered", total;
                     "bv_pruned", bv_pruned;
                     "taint_pruned", taint_pruned;
                     "interval_pruend", interval_pruned;
                     "interproc_pruned", interproc_pruned;
                     "symex_pruned", symex_pruned;
                     "unsupported_prune", unsupported_pruned;]
  in
  let json_body = List.map print_order
                    ~f:(fun (n, v) -> s n @@ stat stats v)
                  |> String.concat ~sep:"\n"
  in
  sprintf "{\n%s}\n" json_body

let info_print statcat headermsg =
  Logs.info ~src (fun m -> m "%s:" headermsg);
  let stats = get statcat in
  Logs.info ~src (fun m ->
    m "%s" @@ to_json_string stats);
