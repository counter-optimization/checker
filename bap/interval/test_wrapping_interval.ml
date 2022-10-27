open Core_kernel
open Bap.Std
open OUnit2

module WI = Wrapping_interval

(** Helpers *)
let one = WI.b1
let zero = WI.b0
let assert_equal = OUnit2.assert_equal ~cmp:WI.equal

(** Test cases *)
let test_add_basic _ = assert_equal (WI.add one zero) one

let test_top_contains_one _ =
  let fail_str = "top does not contain 1!" in
  assert_bool fail_str (WI.contains one WI.top)

(* TODO: test wrap, wrap_intvl, order, join, contain/interval_subset, add, mul, logand, logor, logxor, boollt, boolle, boolslt, boolsle, could_be_true, could_be_false, signed, unsigned, low, high, contains_pow_of_two *)

(** Test suite *)
let suite = "Test_wrapping_interval test suite" >:::
              ["test add basic" >:: test_add_basic;
               "test top contains one basic" >:: test_top_contains_one]
