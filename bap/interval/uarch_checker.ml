open Core_kernel
open Bap_main
open Bap.Std

module UarchCheckerExtension = struct
  let get_target_file_name (proj : Project.t) : string =
    match Project.get proj filename with
    | Some file_name -> file_name
    | None ->
       failwith "In get_target_file_name, couldn't get target file name from project"

  let sub_matches_name target_name (sub : sub term) : bool =
    Sub.name sub |> String.equal target_name

  let get_target_func_from_name ~(name : string) proj =
    let prog = Project.program proj in
    let subs = Term.enum sub_t prog in
    let maybe_sub = Seq.find subs ~f:(sub_matches_name name) in
    match maybe_sub with
    | Some sub -> sub
    | None ->
       let file_name = get_target_file_name proj in
       let err_msg = Format.sprintf
                       "Couldn't find subroutine with name %s in file %s\n"
                       name file_name in
       failwith err_msg

  (* would be a shame to run and wait on the checkers before
     erroring on file access problems *)
  let test_output_csv_file ~(filename : string) : unit =
    let out_ch = Out_channel.create filename ~binary:false ~append:false in
    Out_channel.close out_ch
  
  let pass ctxt proj =
    let target_obj_name = get_target_file_name proj in

    let out_csv_file_name = Extension.Configuration.get ctxt Common.output_csv_file_param in
    let () = test_output_csv_file ~filename:out_csv_file_name in
    
    let i = Image.create ~backend:"llvm" target_obj_name in
    let img_and_errs = match i with
      | Error e ->
         failwith @@
           sprintf "In Uarch_checker.pass, couldn't build image for %s" target_obj_name
      | Ok i -> i in
    let img = fst img_and_errs in

    let config_path = Extension.Configuration.get ctxt Common.config_file_path_param in
    let config = Config.Parser.parse_config_file ~path:config_path in
    let () = printf "Config is:\n%!"; Config.pp config in

    Driver.check_config config img ctxt proj
    
  let register_pass ctxt =
    Project.register_pass' (pass ctxt);
    Ok ()
end

let () = Extension.declare
           ~features:["primus"; "symbolic-executor"; "symbolic-lisp-primitives"]
           UarchCheckerExtension.register_pass
