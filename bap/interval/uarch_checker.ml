open Core_kernel
open Bap_main
open Bap.Std

module UarchCheckerExtension = struct
  let target_func_param = Extension.Configuration.parameter
                            ~doc:"Used to tell the checker which function to check"
                            Extension.Type.string
                            "target-function"

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
    let target_fn = get_target_func target_func_name proj in
    Cfg.check_fn target_fn
    
  let register_pass ctxt =
    Project.register_pass' (pass ctxt);
    Ok ()
end

let () = Extension.declare UarchCheckerExtension.register_pass