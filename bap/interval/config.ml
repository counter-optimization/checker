open Core_kernel
open Bap_main
open Bap.Std

module Theory = Bap_core_theory.Theory
module KB = Bap_core_theory.KB

open KB.Monad_infix

let log_prefix = sprintf "%s.config" Common.package
module L = struct
  include Dolog.Log
  let () = set_prefix log_prefix
end

(* 
   config_file ::= ( init_fn )* ( analyze_fn ) ( analyze_fn )*

   init_fn ::= init func_symbol_name '\n'

   analyze_fn ::= func_symbol_name,secret_arg_idx '\n'

   func_symbol_name ::= <symbol_name_id_string> // no commas in the name

   secret_arg_idx ::= <integer >= 0>
*)

module T = struct
  type symbol = string

  type symbols = (String.t, String.comparator_witness) Set.t

  type secrets = (Int.t, Int.comparator_witness) Set.t

  type secret_map = (string, secrets, String.comparator_witness) Map.t

  type t = {
    init_fns : symbols ;
    target_fns : symbols ;
    secret_args : secret_map
  }

  type function_name = string
  type exn += UnresolvedFunction of function_name

  let empty_secrets = Set.empty (module Int)

  let empty_symbols = Set.empty (module String)

  let empty_secret_map = Map.empty (module String)

  let get_secret_args (c : t) : secret_map = c.secret_args

  let get_taint_arg_indices subname config : int list option =
    let secret_args = config.secret_args in
    match Map.find secret_args subname with
    | Some arg_indices -> Some (Set.to_list arg_indices)
    | None -> None

  let filename proj = Option.value_exn @@ Project.get proj filename

  let try_prog_resolve prog toresolve =
    let emp = Sub.Set.empty in
    let unresolved = String.Set.empty in
    let init = emp, unresolved in
    let subs = Term.enum sub_t prog in
    let named = Seq.map subs ~f:(fun s -> (Sub.name s, s))
                |> String.Map.of_sequence_exn in
    Set.fold toresolve ~init ~f:(fun (res, unres) name ->
      match String.Map.find named name with
      | Some sub -> (Sub.Set.add res sub, unres)
      | None -> (res, String.Set.add unres name))

  let get_all_named_symbols proj toresolve =
    L.info "get_all_named_symbols toresolve is: %s"
        (Set.to_list toresolve |> List.to_string ~f:Fn.id);
    let resolved = ref [] in
    let filename = filename proj in
    (Toplevel.exec begin
       Theory.Unit.for_file filename >>= fun unit ->
       KB.collect Image.Spec.slot unit >>= fun ogre_doc ->
       let query = Ogre.Query.(select @@ from Image.Scheme.named_symbol) in
       match Ogre.eval (Ogre.collect query) ogre_doc with
       | Error err ->
         failwith @@
         sprintf "[Config] named symbols : %s" (Error.to_string_hum err)
       | Ok qr ->
         let targeted = Seq.filter qr
                          ~f:(fun (addr, sym) -> Set.mem toresolve sym)
         in
         Seq.iter targeted ~f:(fun (addr, sym) ->
           L.debug "toresolve: %s, %s" (Int64.to_string addr) sym);
         let is_targeted addr =
           Seq.find targeted ~f:(fun (oaddr, _name) ->
             Int64.equal addr oaddr)
         in
         KB.objects Theory.Program.cls >>= fun tids ->
         KB.Seq.iter tids ~f:(fun tid ->
           KB.collect Theory.Label.addr tid >>= function
           | None -> KB.return ()
           | Some addr ->
             let addr = Bitvec.to_int64 addr in
             match is_targeted addr with
             | None -> KB.return ()
             | Some (_, orig_sym) ->
               L.debug "tid %a has addr %s" Tid.ppo tid (Int64.to_string addr);
               L.debug "that sym is: %s" orig_sym;
               KB.collect Theory.Label.aliases tid >>= fun aliases ->
               L.debug "that tid's aliases are:";
               Set.iter aliases ~f:(fun als ->
                 L.debug "\t%s" als);
               let base_set = List.Assoc.find !resolved orig_sym
                                ~equal:String.equal
                              |> Option.value ~default:(Set.empty (module String)) in
               L.debug "base set is:";
               Set.iter base_set ~f:(L.debug "\t%s");
               let aliases = Set.union base_set aliases in
               KB.return (resolved := (orig_sym, aliases) :: !resolved)) end);
    !resolved


  let get_target_fns_exn config proj : sub term Sequence.t =
    let prog = Project.program proj in
    let toresolve = config.target_fns in
    let resolved, unresolved = try_prog_resolve prog toresolve in
    if String.Set.is_empty unresolved
    then Sub.Set.to_sequence resolved
    else
      let exn = UnresolvedFunction (String.Set.to_list unresolved
                                    |> List.to_string ~f:Fn.id)
      in
      raise exn

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

  let parsed_of_line line : parsed =
    let is_init_fn_line = String.is_prefix line ~prefix:init_fn_prefix in
    if is_init_fn_line
    then
      let init_fn = String.chop_prefix_if_exists line ~prefix:init_fn_prefix in
      InitFn init_fn
    else
      let line_as_list = String.split line ~on:',' in
      let () = if List.length line_as_list <> 2
        then failwith @@ sprintf
                           "In Config.Parser.parsed_of_line, target_fn line %s is malformed"
                           line
        else () in
      let target_fn_sym_name = List.hd_exn line_as_list in
      let secret_idx = List.hd_exn @@ List.tl_exn line_as_list in
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
      { target_fns = empty_symbols;
        init_fns = empty_symbols;
        secret_args = empty_secret_map }
      |> set_target_fns target_fns
      |> set_init_fns init_fns)
end
