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

let test_signed_result_small_intvl_correct _ =
  let i_0_100 = i (0, 100) in
  let res = signed 32 i_0_100 in
  let expected = i ~width:32 (0, 100) in
  
  let res_width = Option.value_exn (get_width res) in
  let exp_width = 32 in
  let () = OUnit2.assert_equal res_width exp_width in

  let () = assert_equal res expected in
  assert_bool "result's sign is wrong" (Option.value_exn (get_sign res))

let test_unsigned_result_small_intvl_correct _ =
  let i_0_100 = i (0, 100) in
  let res = unsigned 32 i_0_100 in
  let expected = i ~width:32 (0, 100) in
  
  let res_width = Option.value_exn (get_width res) in
  let exp_width = 32 in
  let () = OUnit2.assert_equal res_width exp_width in
  
  let () = assert_equal res expected in
  assert_bool "result's sign is wrong" (not (Option.value_exn (get_sign res)))

let test_low_result_big_const_correct _ =
  let c = of_int ~width:64 8 in
  let res = low 3 c in
  let expected = of_int ~width:3 0 in

  let res_width = Option.value_exn (get_width res) in
  let exp_width = 3 in
  let () = OUnit2.assert_equal res_width exp_width in
  
  let () = assert_equal res expected in

  let res_sign = Option.value_exn (get_sign res) in
  let exp_sign = false in
  OUnit2.assert_equal res_sign exp_sign

let test_high_result_small_const_correct _ =
  let c = of_int ~width:64 1 in
  let res = high 1 c in
  let expected = of_int ~width:1 0 in

  let res_width = Option.value_exn (get_width res) in
  let exp_width = 1 in
  let () = OUnit2.assert_equal res_width exp_width in
  
  let () = assert_equal res expected in

  let res_sign = Option.value_exn (get_sign res) in
  let exp_sign = false in
  OUnit2.assert_equal res_sign exp_sign

let test_extract_middle_bits_correct _ =
  let c = of_int ~width:64 14 in
  
  let res = extract c 3 1 in
  let expected = of_int ~width:3 7 in
  let () = assert_equal expected res in

  let res_width = Option.value_exn (get_width res) in
  let exp_width = 3 in
  OUnit2.assert_equal exp_width res_width

let test_concat_two_small_correct _ =
  (* 0b1110.1110 = 238 *)
  let x = of_int ~width:4 14 in
  let y = of_int ~width:4 14 in

  let res = concat x y in
  let expected = of_int ~width:8 238 in
  let () = assert_equal expected res in

  let res_width = Option.value_exn (get_width res) in
  let exp_width = 8 in
  OUnit2.assert_equal exp_width res_width


(* TODO: test wrap, wrap_intvl, order, join, contain/interval_subset, mul, logand, logor, logxor, boollt, boolle, boolslt, boolsle, could_be_true, could_be_false, contains_pow_of_two *)

(** Test suite *)
let suite = "Test_wrapping_interval test suite" >:::
              ["test add basic" >:: test_add_basic;
               "test top contains one basic" >:: test_top_contains_one;
               "test add two const" >:: test_add_two_const;
               "test add two intvls" >:: test_add_two_intvls;
               "test 8 bit wrap" >:: test_8_bit_wrap;
               "test high 1's result bitwidth correct" >:: test_high_1_bitwidth_correct;
               "test signed result correct for small unsigned" >:: test_signed_result_small_intvl_correct;
               "test unsigned result correct for small unsigned" >:: test_unsigned_result_small_intvl_correct;
               "test low result for bigger const correct" >:: test_low_result_big_const_correct;
               "test high result for small const correct" >:: test_high_result_small_const_correct;
               "test extract of middle bits is correct" >:: test_extract_middle_bits_correct;
               "test concat of two small width intvls is correct" >:: test_concat_two_small_correct]
