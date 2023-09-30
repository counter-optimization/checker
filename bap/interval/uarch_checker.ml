open Core_kernel
open Bap_main
open Bap.Std

module KB = Bap_core_theory.KB
module T = Bap_core_theory.Theory

let src = Uc_log.create_src "uarch-checker-main"

module UarchCheckerExtension = struct
  let get_target_file_name proj =
    match Project.get proj filename with
    | Some file_name -> file_name
    | None ->
      failwith "In get_target_file_name, couldn't get target file name from project"

  let sub_matches_name target_name sub =
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
  let test_output_csv_file filename =
    Out_channel.create filename
      ~binary:false
      ~append:false
    |> Out_channel.close

  let init_logging ctxt =
    let lvl = Extension.Configuration.get ctxt
                Common.global_log_level_param
    in
    Uc_log.init ();
    Uc_log.set_global_level lvl

  let pass ctxt proj =
    init_logging ctxt;
    let target_obj_name = get_target_file_name proj in
    let out_csv_file_name = Extension.Configuration.get ctxt
                              Common.output_csv_file_param
    in
    test_output_csv_file out_csv_file_name;
    let i = Image.create ~backend:"llvm" target_obj_name in
    let img_and_errs = match i with
      | Ok i -> i
      | Error e ->
        failwith @@
        sprintf "In Uarch_checker.pass, couldn't build image for %s"
          target_obj_name
    in
    let img = fst img_and_errs in
    let config_path = Extension.Configuration.get ctxt
                        Common.config_file_path_param
    in
    let config = Config.Parser.parse_config_file
                   ~path:config_path
    in
    Logs.info ~src (fun m -> m "Configured:\n");
    Driver.check_config config img ctxt proj

  let register_passes ctxt =
    Project.register_pass' (pass ctxt);
    Ok ()
end

let () =
  Extension.declare
    ~features:["primus"; "symbolic-executor"; "symbolic-lisp-primitives"]
    ~provides:[Common.package; "semantic-addrs"]
    UarchCheckerExtension.register_passes;
  Extension.declare ~provides:["semantic-addrs"] (fun ctxt ->
    Sema_addrs.init (); Ok ())
