open Core
open Bap_main
open Bap.Std

module UarchCheckerExtension = struct
  let target_func_param = Extension.Configuration.parameter
                            ~doc:"Used to tell the checker which function to check"
                            Extension.Type.string
                            "target-function"

  (* TODO: lol i forgot this just comes in through the project
     as the target file name *)
  let target_func_obj_file = Extension.Configuration.parameter
                               Extension.Type.string
                               "target-obj-file"

  let get_target_func name proj =
    let syms = Project.symbols proj in
    let named_tuples = Symtab.to_sequence syms in
    let maybe_target_funcs = Seq.filter named_tuples
                               ~f:(fun (name', _, _) -> String.equal name name') in
    if Seq.length maybe_target_funcs > 1
    then
      let possible_matches = Seq.to_list maybe_target_funcs |> List.to_string ~f:(fun (name, _, _) -> name) in
      let err_str = Printf.sprintf "More than one matching target func found: %s\n%!" possible_matches in
      failwith err_str
    else
      match Seq.hd maybe_target_funcs with
      | Some target -> target
      | None -> failwith "Didn't find a target function with that name"

  let pass ctxt proj =
    let target_func_name = Extension.Configuration.get ctxt target_func_param in
    let target_obj_name = Extension.Configuration.get ctxt target_func_obj_file in
    let target_fn = get_target_func target_func_name proj in
    let () = List.iter (Image.available_backends ()) ~f:(fun b ->
                 Format.printf "Backend: %s\n%!" b) in
    let i = Image.create ~backend:"llvm" target_obj_name in
    let () = match i with
      | Error e ->
         begin
           let () = Format.printf "Couldn't build image for %s\n%!" target_obj_name in
           Format.printf "Error is: %a\n%!" Error.pp e
         end
      | _ -> ()
    in
    (* 0x44A2A8 = 4498088 *)
    let _ = Or_error.(>>=) i @@ fun (i, errs) ->
                                let segs = Image.segments i in
                                let addr = Word.of_int ~width:64 4498088 in
                                let () = Table.iteri segs ~f:(fun m s ->
                                             let n = Image.Segment.name s in
                                             if Memory.contains m addr
                                             then
                                               begin
                                                 let () = Format.printf "Addr is in seg: %s\n%!" n in
                                                 let ptr = Memory.get ~addr ~scale:`r64 m in
                                                 match ptr with
                                                 | Ok p -> Format.printf "Contents of %a is %a\n%!" Word.pp addr Word.pp p
                                                 | Error e ->
                                                    Format.printf "Couldnt get contents of %a: %a\n%!" Word.pp addr Error.pp e
                                               end
                                             else
                                               ())
                                in
                                let d = Image.data i in
                                let is = [0;1;2;3;4] in
                                let data = List.map is ~f:(fun idx ->
                                               Char.to_string @@ Bigstring.get d idx) in
                                let () = List.iter data ~f:(fun s -> Format.printf "%s\n%!" s) in
                                Or_error.return (i, errs)
    in
    Cfg.check_fn target_fn
    
  let register_pass ctxt =
    Project.register_pass' (pass ctxt);
    Ok ()
end

let () = Extension.declare UarchCheckerExtension.register_pass
