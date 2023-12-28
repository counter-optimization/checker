open Core_kernel
open Bap.Std
       
module KB = Bap_knowledge.Knowledge
module T = Bap_core_theory.Theory

type _ key = ..

module type PASS = sig
  type t
  type _ key += Key : t key
  val default : unit -> t
  val onjmp : jmp term -> t -> t
  val ondef : def term -> t -> t
  val onphi : phi term -> t -> t
end

module type GroupedAnalysesS = sig
  val register_runner : 'st. (module PASS with type t = 'st) -> unit
  val run : Blk.t Seq.t -> unit
  val get_final_state : 'a. (module PASS with type t='a) -> 'a
end

type runner =
    Runner : (module PASS with type t = 'a) * 'a -> runner

module DefaultPass : PASS = struct
  type t = bool
  type _ key += Key : t key
  let default () = false
  let onjmp _ _ = false
  let ondef _ _ = false
  let onphi _ _ = false
end

let default_runner = Runner ((module DefaultPass),
                             DefaultPass.default ())

module GroupRunner(Sizer : sig val n : int end) = struct
  type mapper = { run : 'a. (module PASS with type t = 'a) -> 'a -> 'a }
    
  let last = ref 0
               
  let runners = Array.create ~len:Sizer.n default_runner

  type exn += TooManyRunnersRegistered of int
  let check_num_runners () =
    if !last = Sizer.n
    then raise (TooManyRunnersRegistered Sizer.n)
    
  let register_runner (type a) (module Pass : PASS with type t = a) : unit =
    check_num_runners ();
    let r = Runner ((module Pass), Pass.default ()) in
    runners.(!last) <- r;
    Int.incr last

  type ('a, 'b) eq = Eq : ('a, 'a) eq | Neq
  type analysis_name = string
  let check_eq (type a b)
        (module P : PASS with type t = a)
        (module Q : PASS with type t = b) : (a, b) eq =
    match P.Key with
    | Q.Key -> Eq
    | _ -> Neq
    
  let get_final_state (type a) (module Pass : PASS with type t = a) : a =
    let v : a ref = ref (Pass.default ()) in
    for ii = 0 to (Sizer.n - 1) do
      match runners.(ii) with
      | Runner ((module P), st) -> begin
          match check_eq (module P) (module Pass) with
          | Eq -> v := st
          | Neq -> ()
        end
    done;
    !v

  let map {run} : unit =
    for ii = 0 to (Sizer.n - 1) do
      match runners.(ii) with
      | Runner ((module Pass), st) ->
        let st' = run (module Pass) st in
        runners.(ii) <- Runner ((module Pass), st')
    done

  let init_states () =
    map { run = fun (type a)
            (module Pass : PASS with type t = a)
            _ -> Pass.default ()
        }

  let step elt =
    let stepfn = match elt with
      | `Def d -> {
          run = fun (type a)
            (module Pass : PASS with type t=a) st ->
            Pass.ondef d st
        }
      | `Jmp j -> {
          run = fun (type a)
            (module Pass : PASS with type t=a) st ->
            Pass.onjmp j st
        }
      | `Phi p -> {
          run = fun (type a)
            (module Pass : PASS with type t=a) st ->
            Pass.onphi p st
        }
    in
    map stepfn

  let run (blks : Blk.t Seq.t) : unit =
    init_states ();
    Seq.iter blks ~f:(fun blk ->
      Blk.elts blk |> Seq.iter ~f:step);
end

let run_single (type st) (module Pass : PASS with type t = st) (blks : Blk.t Seq.t) : st =
  let init = Pass.default () in
  Seq.map blks ~f:(Blk.elts)
  |> Seq.fold ~init ~f:(fun st elts ->
    Seq.fold elts ~init:st ~f:(fun st -> function
      | `Def d -> Pass.ondef d st
      | `Jmp j -> Pass.onjmp j st
      | `Phi p -> Pass.onphi p st))
                     
module GroupedAnalyses = GroupRunner(struct
    let n = 3
  end)

(* let () = *)
(*   GroupedAnalyses.register_runner *)
(*     (module Dmp_helpers.FindSafePtrBitTestPass); *)
(*   GroupedAnalyses.register_runner *)
(*     (module Idx_calculator.Pass); *)
(*   GroupedAnalyses.register_runner *)
(*     (module Flag_ownership.Pass) *)
