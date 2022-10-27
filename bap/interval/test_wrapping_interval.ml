open Core_kernel
open Bap.Std
open OUnit2

module WI = Wrapping_interval
open WI

(** Helpers *)
let one = b1
let zero = b0
let assert_equal = OUnit2.assert_equal
                     ~cmp:equal
                     ~printer:to_string
let i ?(width = 64) x = of_int_tuple ~width x

(** Test cases *)
let test_add_basic _ = assert_equal (add one zero) one

let test_top_contains_one _ =
  let fail_str = "top does not contain 1!" in
  assert_bool fail_str (contains one top)

let test_add_two_const _ =
  assert_equal (add one zero) one

let test_add_two_intvls _ =
  let zero_ten = i (0, 10) in
  let zero_twenty = i (0, 20) in
  let res = add zero_ten zero_twenty in
  let expected = i (0, 30) in
  assert_equal expected res

let test_8_bit_wrap _ =
  let i0_255 = i ~width:8 (0, 255) in
  let one8 = of_int ~width:8 1 in
  let res = add one8 i0_255 in
  let expected = i0_255 in
  assert_equal expected res

let test_high_1_bitwidth_correct _ =
  let i0_1 = i (0, 1) in
  let high_1 = high 1 i0_1 in
  match get_width high_1 with
  | None -> assert_failure "high_1_bitwidth_correct failed high call"
  | Some w -> OUnit2.assert_equal w 1

(* TODO: test wrap, wrap_intvl, order, join, contain/interval_subset, mul, logand, logor, logxor, boollt, boolle, boolslt, boolsle, could_be_true, could_be_false, signed, unsigned, low, high, contains_pow_of_two *)

(** Test suite *)
let suite = "Test_wrapping_interval test suite" >:::
              ["test add basic" >:: test_add_basic;
               "test top contains one basic" >:: test_top_contains_one;
               "test add two const" >:: test_add_two_const;
               "test add two intvls" >:: test_add_two_intvls;
               "test 8 bit wrap" >:: test_8_bit_wrap;
               "test high 1's result bitwidth correct" >:: test_high_1_bitwidth_correct]
