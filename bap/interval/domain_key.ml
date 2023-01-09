open Core
open Common

module KB = Bap_core_theory.KB

type (_, _) eq =
  | Eq : ('a, 'a) eq
  | Neq

module type KeyT = sig
  type 'a k
  val create : string -> 'a k
  val eq_type : 'a k -> 'b k -> ('a, 'b) eq
  val name : 'a k -> string
end

(* This is the naive external domain key from *)
(* EVA in the Frama-C framework. *)
module DomainKey : KeyT = struct
  type _ key = ..

  module type K = sig
    type t
    type _ key += Key : t key
    val name : string
  end

  type 'a k = (module K with type t = 'a)

  let name (type a) (elt : a k) : string =
    let module A = (val elt : K with type t = a) in
    A.name

  let create (type a) (n : string) : a k =
    let module M = struct
        type t = a
        type _ key += Key : t key
        let name = n
      end in
    (module M : K with type t = a)

  let eq_type (type a) (type b) (x : a k) (y : b k) : (a, b) eq =
    let module A = (val x : K with type t = a) in
    let module B = (val y : K with type t = b) in
    match A.Key with
    | B.Key -> Eq
    | _ -> Neq
end

(* let get_target_out_of_combined *)
(*       (module Combined : NumericDomain) *)
(*       (module Target : NumericDomain) *)
(*     : Combined.t -> Target.t = *)
(*   match Combined.get Target.key with *)
(*   | Some f -> f *)
(*   | None -> *)
(*      let key_name = DomainKey.name Target.key in *)
(*      let combined_name = DomainKey.name Combined.key in *)
(*      failwith @@ *)
(*        sprintf *)
(*          "Couldn't extract %s domain out of domain %s" *)
(*          key_name *)
(*          combined_name *)
