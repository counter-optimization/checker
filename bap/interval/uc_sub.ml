open Core_kernel
open Bap.Std

module KB = Bap_knowledge.Knowledge
module T = Bap_core_theory.Theory
module WI = Wrapping_interval

module Resolve = struct
  open KB.Syntax

  type exn += ResolutionNotInitialized

  let syms = ref Int64.Map.empty
  let initd = ref false
         
  let init proj =
    let filename = Option.value_exn (Project.get proj filename) in
    Toplevel.exec begin
      T.Unit.for_file filename >>= fun unit ->
      KB.collect Image.Spec.slot unit >>= fun doc ->
      let q = Ogre.Query.(select @@ from Image.Scheme.named_symbol) in
      let res = match Ogre.eval (Ogre.collect q) doc with
        | Error err -> failwith @@ Error.to_string_hum err
        | Ok qr -> qr in
      Seq.iter res ~f:(fun (addr,name) ->
        syms := Int64.Map.set !syms ~key:addr ~data:name);
      KB.return ()
    end;
    initd := true

  let resolve (wi : WI.t) : string option =
    if not !initd
    then raise ResolutionNotInitialized
    else match WI.to_int wi with
    | Ok i -> let i = Int64.of_int i in Int64.Map.find !syms i
    | Error err -> failwith @@ Error.to_string_hum err
end
