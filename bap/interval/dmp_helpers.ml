open Core_kernel
open Bap.Std

module KB = Bap_knowledge.Knowledge
module T = Bap_core_theory.Theory

open KB.Monad_infix

type t

let package = "uarch-checker"

let checker_blacklisted_fns = ["smalloc";
                               "sfree";
                               "unsetBit";
                               "smemcpy";
                               "handle64BitStore";
                              ]

let smalloc_addr_clz : (t, unit) KB.cls = KB.Class.declare
                                            ~public:true
                                            ~package
                                            "potential-smalloc-addrs"
                                            ()

let smalloc_addr_slot = KB.Class.property
                          ~public:true
                          ~package
                          smalloc_addr_clz
                          "addr"
                          (KB.Domain.powerset (module Word) "addr-powset")

let smalloc_addr_sym = "smalloc-addr"

let get_smalloc_addrs () =
  KB.Symbol.intern ~package smalloc_addr_sym smalloc_addr_clz >>= fun obj ->
  KB.collect smalloc_addr_slot obj

let print_smalloc_addrs () =
  Toplevel.exec begin
    get_smalloc_addrs () >>= fun addrs ->
    printf "[Dmp_helper] smalloc addrs are:%!";
    Word.Set.iter addrs ~f:(printf "%a, " Word.ppo);
    KB.return (printf "\n%!")
  end

let find_smalloc proj =
  let open Image.Scheme in
  let filename = Option.value_exn (Project.get proj filename) in
  Toplevel.exec begin
    get_smalloc_addrs () >>= fun addrs ->
    if not (Set.is_empty addrs)
    then KB.return ()
    else
      T.Unit.for_file filename >>= fun unit ->
      KB.collect Image.Spec.slot unit >>= fun ogre ->
      let query = Ogre.Query.(select (from external_reference)
                                ~where:(external_reference.(name) =
                                        str "smalloc")) in
      match Ogre.eval (Ogre.collect query) ogre with
      | Ok qr when not (Seq.is_empty qr) ->
        let addrs = Seq.fold qr ~init:addrs ~f:(fun addrs (a, n) ->
          Word.Set.add addrs @@ Word.of_int64 ~width:64 a) in
        KB.Symbol.intern ~package smalloc_addr_sym smalloc_addr_clz >>= fun obj ->
        KB.provide smalloc_addr_slot obj addrs
      | Error err ->
        let e = sprintf "[Dmp_helpers] ogre query error for smalloc : %s"
                  (Error.to_string_hum err) in
        failwith e
      | _ -> failwith "[Dmp_helpers] Couldn't find smalloc virtual address : %s"
  end

let is_smalloc_call (addr : int) : bool =
  let res = ref false in
  Toplevel.exec begin
    get_smalloc_addrs () >>= fun addrs ->
    let addr = Word.of_int ~width:64 addr in
    KB.return (res := Word.Set.mem addrs addr)
  end;
  !res
