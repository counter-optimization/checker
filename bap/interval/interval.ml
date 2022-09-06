open Core_kernel
open Bap.Std

module T = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

let package = "uarch-checker"
let name = "interval"
let desc = "Interval type/domain for abstract interpretation"
let width = 128 (* bits *)

module Endpoint = struct
  type t = Num of Word.t | PosInf | NegInf [@@deriving sexp, bin_io]

  let of_word x = Num x
  let pinf = PosInf
  let ninf = NegInf
  let num x = Num x

  let zero = Num (Word.zero width)
  let one = Num (Word.of_int ~width 1)
  let negone = Num (Word.of_int ~width (-1))

  let (>>=) x f =
    match x with
    | PosInf -> PosInf
    | NegInf -> NegInf
    | Num w -> Num (f w)

  let neg e =
    match e with
    | PosInf -> NegInf
    | NegInf -> PosInf
    | Num w -> Num (Word.neg w)

  let lnot = function
    | PosInf -> zero
    | NegInf -> zero
    | Num x -> Num (Word.lnot x)

  (* is this right? what is +oo + -oo ?? *)
  let add e1 e2 =
    match e1, e2 with
    | PosInf, NegInf -> zero
    | NegInf, PosInf -> zero
    | PosInf, _ -> PosInf
    | _, PosInf -> PosInf
    | NegInf, _ -> NegInf
    | _, NegInf -> NegInf
    | Num x, Num y -> Num (Word.add x y)

  let sub e1 e2 =
    match e1, e2 with
    | PosInf, PosInf -> zero
    | NegInf, NegInf -> zero
    | NegInf, _ -> NegInf
    | PosInf, _ -> PosInf
    | Num x, Num y -> Num (Word.sub x y)
    | Num _, NegInf -> PosInf
    | Num _, PosInf -> NegInf

  let mul e1 e2 =
    match e1, e2 with
    | PosInf, PosInf -> PosInf
    | NegInf, NegInf -> PosInf
    | NegInf, PosInf -> NegInf
    | PosInf, NegInf -> NegInf
    | Num x, Num y -> Num (Word.mul x y)
    | _, PosInf -> PosInf
    | PosInf, _ -> PosInf
    | _, NegInf -> NegInf
    | NegInf, _ -> NegInf

  (* Not caring if e2 is Num 0 *)
  let divide e1 e2 =
    let wzero = Word.zero width in
    match e1, e2 with
    | PosInf, PosInf -> one
    | NegInf, NegInf -> one
    | PosInf, NegInf -> negone
    | NegInf, PosInf -> negone
    | PosInf, Num x -> if Word.(<) x wzero then NegInf else PosInf
    | NegInf, Num x -> if Word.(<) x wzero then PosInf else NegInf
    | Num x, Num y -> Num (Word.div x y)
    | Num _, PosInf -> zero
    | Num _, NegInf -> zero

  let smod e1 e2 =
    match e1, e2 with
    | PosInf, PosInf -> PosInf
    | PosInf, NegInf -> NegInf
    | NegInf, NegInf -> PosInf
    | NegInf, PosInf -> NegInf
    | PosInf, Num x -> e2
    | NegInf, Num x -> neg e2
    | Num x, PosInf -> e1
    | Num x, NegInf -> neg e1
    | Num x, Num y -> Num (Word.modulo x y)

  let lshift e1 e2 =
    match e1, e2 with
    | Num x, Num y -> Num (Word.lshift x y)
    | Num x, PosInf -> zero
    | Num x, NegInf -> zero (* ?? *)
    | _ -> PosInf

  let rshift e1 e2 =
    match e1, e2 with
    | Num x, Num y -> Num (Word.rshift x y)
    | Num x, PosInf -> zero
    | Num x, NegInf -> zero (* ?? *)
    | _ -> PosInf
  
  let arshift e1 e2 =
    match e1, e2 with
    | Num x, Num y -> Num (Word.arshift x y)
    | Num x, PosInf -> zero
    | Num x, NegInf -> zero (* ?? *)
    | _ -> PosInf

  let logand e1 e2 =
    match e1, e2 with
    | Num x, Num y -> Num (Word.logand x y)
    | _, PosInf -> e1
    | PosInf, _ -> e2
    | NegInf, _ -> e2
    | _, NegInf -> e1

  let logor e1 e2 =
    match e1, e2 with
    | Num x, Num y -> Num (Word.logor x y)
    | Num _, PosInf -> PosInf
    | PosInf, PosInf -> PosInf
    | PosInf, Num _ -> PosInf
    | _ -> NegInf

  let logxor e1 e2 =
    match e1, e2 with
    | Num x, Num y -> Num (Word.logxor x y)
    | PosInf, PosInf -> zero
    | NegInf, NegInf -> zero
    | _ -> PosInf

  let smod_single (e : t) (sz : int) : t =
    let max = Int.pow 2 sz in
    let min = (-max) in
    match e with
    | PosInf -> Num (Word.of_int ~width (max - 1))
    | NegInf -> Num (Word.of_int ~width min)
    | Num w ->
       let m = Word.of_int ~width sz in
       Num (Word.modulo w m)

  (** Ordering *)
  let compare (x : t) (y : t) : int =
    match (x, y) with
    | (PosInf, PosInf) -> 0
    | (PosInf, _) -> 1
    | (_, PosInf) -> -1
    | (NegInf, NegInf) -> 0
    | (NegInf, _) -> -1
    | (_, NegInf) -> 1
    | (Num lo, Num hi) -> Word.compare lo hi

  let compare_poset (x : t) (y : t) : KB.Order.partial =
    let open KB.Order in 
    match compare x y with
    | -1 -> LT
    | 0 -> EQ
    | 1 -> GT
    | _ -> failwith "compare_poset unexpected value"

  let max x y =
    match compare x y with
    | 0 -> x
    | -1 -> y
    | 1 -> x
    | _ -> failwith "max unexpected value"

  let min x y =
    match compare x y with
    | 0 -> x
    | -1 -> x
    | 1 -> y
    | _ -> failwith "max unexpected value"

  let max4 a b c d =
    max a b |> max c |> max d

  let min4 a b c d =
    min a b |> min c |> min d

  let to_string x =
    match x with
    | PosInf -> "+oo"
    | NegInf -> "-oo"
    | Num w -> Word.to_string w
end

type t = Bot | Interval of Endpoint.t * Endpoint.t [@@deriving sexp, bin_io]

let empty = Bot
let bot = Bot
let top = Interval (NegInf, PosInf)
let w = Word.of_int ~width

(** Conversion functions *)
let of_ints (lo : int) (hi : int) : t =
  let lo' = Endpoint.of_word (w lo) in
  let hi' = Endpoint.of_word (w hi) in
  Interval (lo', hi')

let of_int lo_and_hi = of_ints lo_and_hi lo_and_hi

let of_words lo hi = Interval (Endpoint.of_word lo, Endpoint.of_word hi)

let of_word lo_and_hi = of_words lo_and_hi lo_and_hi

let to_string = function
  | Bot -> "_|_"
  | Interval (lo, hi) ->
     let lo = Endpoint.to_string lo in
     let hi = Endpoint.to_string hi in
     Format.sprintf "[%s, %s]" lo hi

let b0 = of_int 0
let b1 = of_int 1
  
(** Comparison/Domain *)
let order x y : KB.Order.partial =
  let open KB.Order in
  match (x, y) with
  | Bot, Bot -> EQ
  | Bot, _ -> LT
  | _, Bot -> GT
  | Interval (lo, hi), Interval (lo', hi') ->
     let lows = Endpoint.compare lo lo' in
     let highs = Endpoint.compare hi hi' in
     match (lows, highs) with
     | 0, 0 -> EQ (* the same interval *)
     | -1, 0 -> GT (* GT, e.g., [-4, 0] >= [-3, 0] bc ordering is inclusion *)
     | -1, 1 -> GT
     | 0, 1 -> GT
     | 0, -1 -> LT (* [0, 1] <= [0, 2] *)
     | 1, -1 -> LT
     | 1, 0 -> LT
     | -1, -1 -> NC
     | 1, 1 -> NC
     | _ -> failwith "unhandled case in comparing intervals"

let equal (x : t) (y : t) : bool =
  match order x y with
  | KB.Order.EQ -> true
  | _ -> false

let join x y =
  match x, y with
  | Bot, Bot -> Bot
  | Bot, _ -> y
  | _, Bot -> x
  | Interval (lo, hi), Interval (lo', hi') ->
     Interval (Endpoint.min lo lo', Endpoint.max hi hi')

let to_string x =
  match x with
  | Bot -> "_|_"
  | Interval (lo, hi) ->
     let lo = Endpoint.to_string lo in
     let hi = Endpoint.to_string hi in
     Format.sprintf "[%s, %s]" lo hi

(** Operators *)
let unop f = function
  | Bot -> Bot
  | Interval (lo, hi) ->
     let nlo = f lo in
     let nhi = f hi in
     let smallest = Endpoint.min nlo nhi in
     let biggest = Endpoint.max nlo nhi in
     Interval (smallest, biggest)
  
let neg = unop Endpoint.neg
let lnot = unop Endpoint.lnot

let binop f i1 i2 =
  match i1, i2 with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Interval (lo, hi), Interval (lo', hi') ->
     let x1 = f lo lo' in
     let x2 = f lo hi' in
     let x3 = f hi lo' in
     let x4 = f hi hi' in
     let min = Endpoint.min4 x1 x2 x3 x4 in
     let max = Endpoint.max4 x1 x2 x3 x4 in
     Interval (min, max)

let low n = function
  | Bot -> Bot
  | Interval (lo, hi) ->
     let x = Endpoint.smod_single lo n in
     let y = Endpoint.smod_single hi n in
     let smallest = Endpoint.min x y in
     let biggest = Endpoint.max x y in
     Interval (smallest, biggest)

(* This could be more precise *)
let high n = function
  | Bot -> Bot
  | Interval (lo, hi) -> top
     
let unsigned n x = x
let signed n x = x
               
let add = binop Endpoint.add
let sub = binop Endpoint.sub
let mul = binop Endpoint.mul
let div = binop Endpoint.divide
let sdiv = binop Endpoint.divide
let umod x y =
  match x, y with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Interval (lo, hi), Interval (lo', hi') ->
     let lo = Endpoint.zero in
     let hi = Endpoint.max lo' hi' in
     Interval (lo, hi)
let smod = binop Endpoint.smod
let lshift = binop Endpoint.lshift
let rshift = binop Endpoint.rshift
let arshift = binop Endpoint.arshift
let logand = binop Endpoint.logand
let logor = binop Endpoint.logor
let logxor = binop Endpoint.logxor

let booleq x y =
  if equal x y then b1 else b0

let boolneq x y =
  if equal x y then b0 else b1

let boollt x y =
  match order x y with
  | LT -> b1
  | _ -> b0

let boolle x y =
  match order x y with
  | LT -> b1
  | EQ -> b1
  | _ -> b0

let boolslt x y =
  match order x y with
  | LT -> b1
  | _ -> b0

let boolsle x y =
  match order x y with
  | LT -> b1
  | EQ -> b1
  | _ -> b0

let could_be_true x =
  match order b1 x with
  | LT -> true
  | EQ -> true
  | _ -> false

(** KB related declares and defines *)
let domain =
  KB.Domain.define
    ~inspect:sexp_of_t
    ~join:(fun x y -> Ok (join x y))
    ~empty
    ~order
    "interval-domain"

(* type endpoint = Inf | Bot | Num of Word.t [@@deriving sexp] *)

(* type t = { *)
(*     refinements: Word.t; *)
(*     low: endpoint; *)
(*     high: endpoint; *)
(*   } [@@deriving sexp] *)

(* let width : int = 128 *)
(* let refinement_threshold : Word.t = Word.of_int ~width 1024 *)

(* let (>>=) (x : endpoint) (f : Word.t -> endpoint) : endpoint = *)
(*   match x with *)
(*   | Inf -> Inf *)
(*   | Bot -> Bot *)
(*   | Num x -> f x *)

(* (\** Functions for creating intervals *\) *)
(* let bottom = { *)
(*     refinements = Word.of_int ~width 0; *)
(*     low = Bot; *)
(*     high = Bot; *)
(*   } *)

(* let top = { *)
(*     refinements = Word.of_int ~width 0; *)
(*     low = Inf; *)
(*     high = Inf; *)
(*   } *)

(* let of_ints (low : int) (high : int) : t = *)
(*   { refinements = Word.of_int ~width 0; *)
(*     low = Num (Word.of_int ~width low); *)
(*     high = Num (Word.of_int ~width high); *)
(*   } *)

(* let of_int n = of_ints n n *)

(* let of_words (low : Word.t) (high : Word.t) : t = *)
(*   { refinements = Word.of_int ~width 0; *)
(*     low = Num low; *)
(*     high = Num high; *)
(*   } *)

(* let of_word n = of_words n n *)

(* (\** pp and string *\) *)
(* let endpoint_to_string = function *)
(*   | Inf -> "Inf" *)
(*   | Bot -> "Bot" *)
(*   | Num x -> Word.to_string x *)

(* let to_string (x : t) : string = *)
(*   let low = endpoint_to_string x.low in *)
(*   let high = endpoint_to_string x.high in *)
(*   Format.sprintf "[%s, %s]" low high *)

(* let pp ppf x = *)
(*   Format.fprintf ppf "%s" (to_string x) *)

(* (\** Interval ops *\) *)
(* module Order : KB.Order.S with type t := t = struct *)
  
(*   let order x y : KB.Order.partial = *)
(*     let module O = KB.Order in *)
(*     match (x.low, x.high, y.low, y.high) with *)
(*     | (Bot, Bot, Bot, Bot) -> O.EQ *)
(*     | (Inf, Inf, Inf, Inf) -> O.EQ *)
(*     | (Num a, Num b, Num c, Num d) -> *)
(*        begin *)
(*          match (Word.compare a c, Word.compare b d) with *)
(*          | (0, 0) -> O.EQ *)
(*          | (-1, 0) -> O.GT *)
(*          | (0, -1) -> O.LT *)
(*          | (1, 0) -> O.LT *)
(*          | (0, 1) -> O.GT *)
(*          | (1, 1) -> O.NC *)
(*          | (-1, -1) -> O.NC *)
(*          | (-1, 1) -> O.GT *)
(*          | (1, -1) -> O.LT *)
(*          | _ -> failwith "Impossible value" *)
(*        end *)
(*     | (Bot, Bot, _, _) -> O.LT *)
(*     | (Inf, Inf, _, _) -> O.GT *)
(*     | (_, _, Bot, Bot) -> O.GT *)
(*     | (_, _, Inf, Inf) -> O.LT *)
(*     | _ -> *)
(*         let err_str = Format.asprintf "Order not implemented for case %a %a" pp x pp y in *)
(*         failwith err_str *)
(* end *)

(* let join_endpoint (pick : Word.t -> Word.t -> Word.t) (x : endpoint) (y : endpoint) : endpoint = *)
(*   match (x, y) with *)
(*   | (Bot, _) -> y *)
(*   | (_, Bot) -> x *)
(*   | (Inf, _) -> x *)
(*   | (_, Inf) -> y *)
(*   | (Num a, Num b) -> Num (pick a b) *)
                           
(* let join_low = join_endpoint Word.min *)
(* let join_high = join_endpoint Word.max *)

(* let is_bot x : bool = *)
(*   match (x.low, x.high) with *)
(*   | Bot, Bot -> true *)
(*   | _ -> false *)

(* let is_top x : bool = *)
(*   match (x.low, x.high) with *)
(*   | Inf, Inf -> true *)
(*   | _ -> false *)

(* let inc_refinement (prev : Word.t) : Word.t = *)
(*   Word.add prev (Word.one width) *)

(* (\* with refinements, this has the semantics of x = x `join` y *\) *)
(* let join x y : (t, KB.Conflict.t) Core_kernel.result = *)
(*   if Word.(x.refinements >= refinement_threshold) *)
(*   then Ok top *)
(*   else Ok { *)
(*     refinements = inc_refinement x.refinements; *)
(*     low = join_low x.low y.low; *)
(*     high = join_high x.high y.high; *)
(*   } *)


(* let binop (f : Word.t -> Word.t -> Word.t) (i1 : t) (i2 : t) : t = *)
(*   { refinements = Word.max i1.refinements i2.refinements; *)
(*     low = i1.low >>= (fun x -> *)
(*           i2.low >>= (fun y -> *)
(*           Num (f x y))); *)
(*     high = i1.high >>= fun x -> *)
(*            i2.high >>= fun y -> *)
(*            Num (f x y) *)
(*   } *)

(* let add = binop Word.add *)
(* let sub = binop Word.sub *)
(* let mul = binop Word.mul *)
(* let div = binop Word.div *)

(* (\** Get all var names *\) *)
(* module VarNameSet = Set.Make_binable_using_comparator(String) *)

(* module VarScraper = struct *)
(*   type t = VarNameSet.t *)

(*   let order set1 set2 : KB.Order.partial = *)
(*     if VarNameSet.equal set1 set2 *)
(*     then KB.Order.EQ *)
(*     else *)
(*       if VarNameSet.is_subset set1 ~of_:set2 *)
(*       then KB.Order.LT *)
(*       else *)
(*         if VarNameSet.is_subset set2 ~of_:set1 *)
(*         then KB.Order.GT *)
(*         else KB.Order.NC *)

(*   let join = VarNameSet.union *)

(*   let var_name_domain = *)
(*     KB.Domain.define *)
(*       "var-name-set" *)
(*       ~empty:VarNameSet.empty *)
(*       ~join:(fun set1 set2 -> Ok (join set1 set2)) *)
(*       ~order:order *)
(*       ~inspect:VarNameSet.sexp_of_t *)

(*   let cls : (t, unit) KB.cls = *)
(*     KB.Class.declare ~package "var-names" () *)

(*   let result_slot = *)
(*     KB.Class.property cls "var-names-result" var_name_domain *)

(*   let s = VarNameSet.singleton *)
(*   let n = T.Var.name *)
(*   let vs = T.Var.sort *)
(*   let sn x = s (n x) *)
(*   let null = T.Value.empty *)
(*   let empty_of_sort x = null (vs x) *)
(*   let empty_eff = T.Effect.empty T.Effect.Sort.bot *)
  
(*   let expr_slot : (T.Value.cls, t) KB.slot = *)
(*     KB.Class.property T.Value.cls "var-name-expr-slot" var_name_domain *)
(*       ~persistent:(KB.Persistent.of_binable (module VarNameSet)) *)

(*   let stmt_slot : (T.Effect.cls, t) KB.slot = *)
(*     KB.Class.property T.Effect.cls "var-name-eff-slot" var_name_domain *)
(*       ~persistent:(KB.Persistent.of_binable (module VarNameSet)) *)
  
(*   module Theory : T.Core = struct *)
(*     include T.Empty *)

(*     open KB.Monad_infix *)

(*     let var (var : 'a T.Var.t) : 'a T.Value.t KB.t = *)
(*       let v = sn var in *)
(*       KB.Value.put expr_slot (empty_of_sort var) v *)
(*       |> KB.return *)
    
(*     let set (var : 'a T.Var.t) (expr : 'a T.Value.t KB.t) *)
(*         : T.data T.Effect.t KB.t = *)
(*       expr >>= fun expr -> *)
(*       let lhs = sn var in *)
(*       let rhs = KB.Value.get expr_slot expr in *)
(*       KB.Value.put stmt_slot empty_eff (join lhs rhs) *)
(*       |> KB.return *)

(*     let seq (p1 : 'a T.Effect.t KB.t) (p2 : 'a T.Effect.t KB.t) *)
(*         : 'a T.Effect.t KB.t = *)
(*       p1 >>= fun p1 -> *)
(*       p2 >>= fun p2 -> *)
(*       let sem1 = KB.Value.get stmt_slot p1 in *)
(*       let sem2 = KB.Value.get stmt_slot p2 in *)
(*       KB.Value.put stmt_slot empty_eff (join sem1 sem2) *)
(*       |> KB.return *)

(*     let blk (label : T.Label.t) (data : T.data T.Effect.t KB.t) *)
(*           (ctrl : T.ctrl T.Effect.t KB.t) : unit T.Effect.t KB.t = *)
(*       data >>= fun data -> *)
(*       ctrl >>= fun ctrl -> *)
(*       let sem1 = KB.Value.get stmt_slot data in *)
(*       let sem2 = KB.Value.get stmt_slot ctrl in *)
(*       let combined = join sem1 sem2 in *)
(*       KB.Value.put stmt_slot empty_eff combined *)
(*       |> KB.return *)
(*   end *)

(*   let desc = "Var name collection semantics" *)

(*   let () = *)
(*     let theory = KB.return (module Theory : T.Core) in *)
(*     T.declare ~package ~desc ~name:"var-name-set" theory *)

(*   let collect_names : t KB.Object.t KB.t = *)
(*     let open KB.Syntax in *)
(*     let vars_of_prog (p : T.program KB.obj) : VarNameSet.t KB.t = *)
(*       p --> T.Semantics.slot >>= fun sem -> *)
(*       KB.return @@ KB.Value.get stmt_slot sem *)
(*     in *)
(*     let all_vars = *)
(*       KB.join *)
(*         (KB.objects T.Program.cls >>= fun progs -> *)
(*          KB.return *)
(*            (Seq.fold progs *)
(*               ~init:(KB.return VarNameSet.empty) *)
(*               ~f:(fun acc prog -> *)
(*                 acc >>= fun acc -> *)
(*                 vars_of_prog prog >>= fun vars -> *)
(*                 let merged = join acc vars in *)
(*                 KB.return merged))) *)
(*     in *)
(*     all_vars >>= fun vars -> *)
(*     KB.Object.create cls >>= fun result_obj -> *)
(*     KB.provide result_slot result_obj vars >>= fun _ -> *)
(*     KB.return result_obj *)

(*   let print_names state = *)
(*     match KB.run cls collect_names state with *)
(*     | Ok (name_vals, _) -> *)
(*        Format.printf "%a\n%!" KB.Value.pp name_vals *)
(*     | Error e -> failwith "error printing names" *)
(* end *)

(* (\** Setting up intervals in the knowledge base *\) *)
(* let interval_domain : t KB.Domain.t = *)
(*   KB.Domain.define *)
(*     name *)
(*     ~inspect:sexp_of_t *)
(*     ~join:join *)
(*     ~empty:bottom *)
(*     ~order:Order.order *)

(* module IntervalAbsInt : T.Core = struct *)
(*   include T.Empty *)

(*   open KB.Monad_infix *)

(*   let empty = T.Effect.empty T.Effect.Sort.bot *)
(*   let null_of s = T.Value.empty s *)
(* end *)

(* module Configuration = struct *)
(*   module Conf = Bap_main.Extension.Configuration *)
(*   module Param_type = Bap_main.Extension.Type *)

(*   let print_kb () = *)
(*     let open KB.Monad_infix in *)
(*     let state = Toplevel.current () in *)
(*     let print_exprs v = *)
(*       match KB.run T.Value.cls (KB.return v) state with *)
(*       | Ok (snapshot, newstate) -> *)
(*          Format.eprintf "- Value: %a\n%!" KB.Value.pp snapshot *)
(*       | Error e -> Format.eprintf "KB problem: %a\n%!" KB.Conflict.pp e *)
(*     in *)
(*     let print_stmts v = *)
(*       match KB.run T.Effect.cls (KB.return v) state with *)
(*       | Ok (snapshot, newstate) -> *)
(*          Format.eprintf "- Value: %a\n%!" KB.Value.pp snapshot *)
(*       | Error e -> Format.eprintf "KB problem: %a\n%!" KB.Conflict.pp e *)
(*     in *)
(*     KB.objects T.Value.cls >>= fun exp_objs -> *)
(*     KB.objects T.Effect.cls >>= fun eff_objs -> *)
(*     Format.printf "Printing exprs and stmts\n%!"; *)
(*     Seq.iter exp_objs ~f:print_exprs; *)
(*     KB.return @@ Seq.iter eff_objs ~f:print_stmts *)
(* end *)

(* let main _ = *)
(*   VarScraper.print_names @@ Toplevel.current () *)

(* let () = *)
(*   Bap_main.Extension.declare @@ fun _ctxt -> *)
(*                                 Project.register_pass' main; *)
(*                                 Ok () *)
