open Core
open Bap_main
open Bap.Std

open Common

(* 
config_file ::= ( init_fn )* ( analyze_fn ) ( analyze_fn )*

init_fn ::= init func_symbol_name '\n'

analyze_fn ::= func_symbol_name,secret_arg_idx '\n'

func_symbol_name ::= <symbol_name_id_string> // no commas in the name

secret_arg_idx ::= <integer >= 0>

 *)

module T = struct
  type symbols = (String.t, String.comparator_witness) Set.t

  type secrets = (Int.t, Int.comparator_witness) Set.t

  type secret_map = (string, secrets, String.comparator_witness) Map.t
  
  type t = { init_fns : symbols ;
             target_fns : symbols ;
             secret_args : secret_map }

  let empty_secrets = Set.empty (module Int)

  let empty_symbols = Set.empty (module String)

  let empty_secret_map = Map.empty (module String)

  let pp c =
    printf "init_fns: %s\n%!" @@ List.to_string ~f:(fun x -> x) (Set.to_list c.init_fns);
    printf "target_fns: %s\n%!" @@ List.to_string ~f:(fun x -> x) (Set.to_list c.target_fns);
    printf "secrets_args:\n%!";
    Map.iteri c.secret_args ~f:(fun ~key ~data ->
        printf "%s -> %s\n%!" key (List.to_string ~f:Int.to_string (Set.to_list data)))
end

include T

module Parser = struct
  
  type path = string
  
  type parsed = InitFn of string | TargetFn of string * string

  let is_init_fn = function
    | InitFn _ -> true
    | TargetFn _ -> false

  let is_target_fn = function
    | InitFn _ -> false
    | TargetFn _ -> true

  let init_fn_prefix = "init "

  let get_path ctxt : path =
    Extension.Configuration.get ctxt Common.config_file_path_param

  let parsed_of_line line : parsed =
    let is_init_fn_line = String.is_prefix line ~prefix:init_fn_prefix in
    if is_init_fn_line
    then
      let init_fn = String.chop_prefix_if_exists line ~prefix:init_fn_prefix in
      InitFn init_fn
    else
      (* let comma_idx = match String.index line ',' with *)
      (*   | Some idx -> idx *)
      (*   | None -> failwith @@ *)
      (*               sprintf *)
      (*                 "In Config.Parser.parsed_of_line, target_fn line %s is malformed" *)
      (*                 line in *)
      let line_as_list = String.split line ~on:',' in
      let () = if List.length line_as_list <> 2
               then failwith @@ sprintf
                                  "In Config.Parser.parsed_of_line, target_fn line %s is malformed"
                                  line
               else () in
      let target_fn_sym_name = List.hd_exn line_as_list in
      let secret_idx = List.hd_exn @@ List.tl_exn line_as_list in
      (* let secret_idx = String.drop_prefix line (comma_idx + 1) in *)
      (* let target_fn_sym_name = String.drop_suffix line (comma_idx + 1) in *)
      let () = printf "Target fn line: secret idx is %s, target_fn is %s\n%!"
                 secret_idx target_fn_sym_name in
      TargetFn (target_fn_sym_name, secret_idx)

  let set_target_fns targets (config : T.t) : T.t =
    let split_of_target target : string * string =
      match target with
      | InitFn _ -> failwith "In Config.Parser.targets_and_secrets_of_target_fns, InitFn is not TargetFn"
      | TargetFn (sym, sec) -> (sym, sec) in
    let (syms, map) =
      List.fold targets ~init:(empty_symbols, empty_secret_map) ~f:(fun (syms, map) target ->
          let sym, sec = split_of_target target in
          let sec_idx_int = Int.of_string sec in
          let syms' = Set.add syms sym in
          let old_indices = Map.find map sym |> Option.value ~default:empty_secrets in
          let new_indices = Set.add old_indices sec_idx_int in
          let map' = Map.set map ~key:sym ~data:new_indices in
          (syms', map')) in
    { config with target_fns = syms ; secret_args = map }

  let set_init_fns inits config : T.t =
    let get_symbol = function
      | InitFn sym -> sym
      | TargetFn _ ->
         failwith "In Config.Parser.targets_and_secrets_of_target_fns, TargetFn is not InitFn" in
    let inits = List.fold inits ~init:empty_symbols ~f:(fun syms init_fn ->
                    Set.add syms @@ get_symbol init_fn) in
    { config with init_fns = inits }

  let parse_config_file ~(path : path) : T.t =
    In_channel.with_file path ~binary:false ~f:(fun inchnl ->
        let lines = In_channel.input_lines inchnl in
        let parsed_lines = List.map lines ~f:parsed_of_line in
        let init_fns = List.filter parsed_lines ~f:is_init_fn in
        let target_fns = List.filter parsed_lines ~f:is_target_fn in
        { target_fns = empty_symbols; init_fns = empty_symbols; secret_args = empty_secret_map }
        |> set_target_fns target_fns
        |> set_init_fns init_fns)
end
