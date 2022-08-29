open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

let package = "checker"
let name = "interval"
let desc = "Interval type/domain for abstract interpretation"

type endpoint = Inf | Bot | Num of Word.t [@@deriving sexp]

type t = {
    refinements: Word.t;
    low: endpoint;
    high: endpoint;
  } [@@deriving sexp]

let width : int = 128
let refinement_threshold : Word.t = Word.of_int ~width 1024

let (>>=) (x : endpoint) (f : Word.t -> endpoint) : endpoint =
  match x with
  | Inf -> Inf
  | Bot -> Bot
  | Num x -> f x

(** Functions for creating intervals *)
let bottom = {
    refinements = Word.of_int ~width 0;
    low = Bot;
    high = Bot;
  }

let top = {
    refinements = Word.of_int ~width 0;
    low = Inf;
    high = Inf;
  }

let of_ints (low : int) (high : int) : t =
  { refinements = Word.of_int ~width 0;
    low = Num (Word.of_int ~width low);
    high = Num (Word.of_int ~width high);
  }

let of_int n = of_ints n n

let of_words (low : Word.t) (high : Word.t) : t =
  { refinements = Word.of_int ~width 0;
    low = Num low;
    high = Num high;
  }

let of_word n = of_words n n

(** pp and string *)
let endpoint_to_string = function
  | Inf -> "Inf"
  | Bot -> "Bot"
  | Num x -> Word.to_string x

let to_string (x : t) : string =
  let low = endpoint_to_string x.low in
  let high = endpoint_to_string x.high in
  Format.sprintf "[%s, %s]" low high

let pp ppf x =
  Format.fprintf ppf "%s" (to_string x)

(** Interval ops *)
module Order : KB.Order.S with type t := t = struct
  
  let order x y : KB.Order.partial =
    let module O = KB.Order in
    match (x.low, x.high, y.low, y.high) with
    | (Bot, Bot, Bot, Bot) -> O.EQ
    | (Inf, Inf, Inf, Inf) -> O.EQ
    | (Num a, Num b, Num c, Num d) ->
       begin
         match (Word.compare a c, Word.compare b d) with
         | (0, 0) -> O.EQ
         | (-1, 0) -> O.GT
         | (0, -1) -> O.LT
         | (1, 0) -> O.LT
         | (0, 1) -> O.GT
         | (1, 1) -> O.NC
         | (-1, -1) -> O.NC
         | (-1, 1) -> O.GT
         | (1, -1) -> O.LT
         | _ -> failwith "Impossible value"
       end
    | (Bot, Bot, _, _) -> O.LT
    | (Inf, Inf, _, _) -> O.GT
    | (_, _, Bot, Bot) -> O.GT
    | (_, _, Inf, Inf) -> O.LT
    | _ ->
        let err_str = Format.asprintf "Order not implemented for case %a %a" pp x pp y in
        failwith err_str
end

let join_endpoint (pick : Word.t -> Word.t -> Word.t) (x : endpoint) (y : endpoint) : endpoint =
  match (x, y) with
  | (Bot, _) -> y
  | (_, Bot) -> x
  | (Inf, _) -> x
  | (_, Inf) -> y
  | (Num a, Num b) -> Num (pick a b)
                           
let join_low = join_endpoint Word.min
let join_high = join_endpoint Word.max

let is_bot x : bool =
  match (x.low, x.high) with
  | Bot, Bot -> true
  | _ -> false

let is_top x : bool =
  match (x.low, x.high) with
  | Inf, Inf -> true
  | _ -> false

let inc_refinement (prev : Word.t) : Word.t =
  Word.add prev (Word.one width)

(* with refinements, this has the semantics of x = x `join` y *)
let join x y : (t, KB.Conflict.t) Core_kernel.result =
  if Word.(x.refinements >= refinement_threshold)
  then Ok top
  else Ok {
    refinements = inc_refinement x.refinements;
    low = join_low x.low y.low;
    high = join_high x.high y.high;
  }


let binop (f : Word.t -> Word.t -> Word.t) (i1 : t) (i2 : t) : t =
  { refinements = Word.max i1.refinements i2.refinements;
    low = i1.low >>= (fun x ->
          i2.low >>= (fun y ->
          Num (f x y)));
    high = i1.high >>= fun x ->
           i2.high >>= fun y ->
           Num (f x y)
  }

let add = binop Word.add
let sub = binop Word.sub
let mul = binop Word.mul
let div = binop Word.div

(** Setting up intervals in the knowledge base *)
type tag = Interval
type sort = unit
let index = () (* index into sort *)

let cls : (tag, sort) KB.cls =
  KB.Class.declare ~package ~desc name index

let interval_domain : t KB.Domain.t =
  KB.Domain.define
    name
    ~inspect:sexp_of_t
    ~join:join
    ~empty:bottom
    ~order:Order.order

let expr : (T.Value.cls, t) KB.slot =
  let name = "interval-absint-expr-slot" in
  KB.Class.property T.Value.cls name interval_domain

let stmt : (T.Semantics.cls, t) KB.slot =
  let name = "interval-absint-stmt-slot" in
  KB.Class.property T.Semantics.cls name interval_domain

module IntervalAbsInt : T.Core = struct
  include T.Empty

  open KB.Monad_infix

  let empty = T.Effect.empty T.Effect.Sort.bot
  let null_of s = T.Value.empty s

  let int (sort : 's T.Bitv.t T.Value.sort) (bv : Bitvec.t)
      : 's T.Bitv.t T.Value.t KB.t =
    let semantics = Bitvec.to_int64 bv |> Word.of_int64 ~width |> of_word in
    let snapshot = KB.Value.put expr (null_of sort) semantics in
    KB.return snapshot

  let var (var : 'a T.Var.t) : 'a T.Value.t KB.t =
    KB.return @@ KB.Value.put expr (null_of @@ T.Var.sort var) bottom

  let set (var : 'a T.Var.t) (rhs : 'a T.Value.t KB.t)
      : T.data T.Effect.t KB.t =
    rhs >>= fun rhs ->
    let evald_rhs = KB.Value.get expr rhs in
    KB.Value.put stmt empty evald_rhs
    |> KB.return

   (* let blk (label : T.Label.t) (data : T.data T.Effect.t KB.t) *)
   (*      (ctrl : T.ctrl T.Effect.t KB.t) : unit T.Effect.t KB.t = *)
   (*  data >>= fun data -> *)
   (*  ctrl >>= fun ctrl -> *)

  let seq (p1 : 'a T.Effect.t KB.t) (p2 : 'a T.Effect.t KB.t)
      : 'a T.Effect.t KB.t =
    p1 >>= fun p1 ->
    p2 >>= fun p2 ->
    let state1 = KB.Value.get stmt p1 in
    let state2 = KB.Value.get stmt p2 in
    match join state1 state2 with
    | Ok merged -> KB.return @@ KB.Value.put stmt empty merged
    | Error e -> failwith "error merging seq states"
end

module Configuration = struct
  module Conf = Bap_main.Extension.Configuration
  module Param_type = Bap_main.Extension.Type

  let addr = Conf.parameter Param_type.string "addr"

  let explore (addr : Bitvec.t) : unit =
    let label = T.Label.for_addr addr in
    let semantics = Toplevel.eval T.Semantics.slot label in
    Format.printf "%a\n%!" KB.Value.pp semantics

  let pass (ctxt : Bap_main.ctxt) (proj : Project.t) : unit =
    let addr = Conf.get ctxt addr in
    let () =
      if String.is_empty addr
        then failwith "No address specified"
        else ()
    in
    let word = Bitvec.of_string addr in
    explore word
  
  let run ctxt =
    let theory = KB.return (module IntervalAbsInt : T.Core) in
    T.declare ~package ~desc ~name:"interval-abs-int" theory;
    Project.register_pass' (pass ctxt);
    Ok ()
end

let () =
  let x = of_ints 14 32 in
  let y = of_int 1 in
  let addxy = add x y in
  let () = Format.printf "%a\n%!" pp addxy in
  let interval_domain_name = KB.Domain.name interval_domain in
  Format.printf "Domain name: %s\n%!" interval_domain_name;
  Bap_main.Extension.declare Configuration.run
