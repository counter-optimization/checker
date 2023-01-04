open Core_kernel
open Bap_main
open Bap.Std

module UarchCheckerExtension = struct
  let target_func_param = Extension.Configuration.parameter
                            ~doc:"Which top-level function to check"
                            Extension.Type.string
                            "target-function"

  let get_target_file_name (proj : Project.t) : string =
    match Project.get proj filename with
    | Some file_name -> file_name
    | None ->
       failwith "In get_target_file_name, couldn't get target file name from project"

  let sub_matches_name target_name (sub : sub term) : bool =
    Sub.name sub |> String.equal target_name

  let get_target_func name proj =
    let prog = Project.program proj in
    let subs = Term.enum sub_t prog in
    let maybe_sub = Seq.find subs ~f:(sub_matches_name name) in
    match maybe_sub with
    | Some sub -> sub
    | None ->
       let file_name = get_target_file_name proj in
       let err_msg = Format.sprintf
                       "Couldn't find subroutine with name %s in file %s\n"
                       name file_name
       in
       failwith err_msg
  
  let pass ctxt proj =
    let target_func_name = Extension.Configuration.get ctxt target_func_param in
    let target_obj_name = get_target_file_name proj in
    let target_fn = get_target_func target_func_name proj in
    let i = Image.create ~backend:"llvm" target_obj_name in
    let img_and_errs = match i with
      | Error e ->
         failwith @@ sprintf "Couldn't build image for %s" target_obj_name
      | Ok i -> i
    in
    let img = fst img_and_errs in
    Driver.check_fn target_fn img ctxt proj
    
  let register_pass ctxt =
    Project.register_pass' (pass ctxt);
    Ok ()
end

let () = Extension.declare UarchCheckerExtension.register_pass
