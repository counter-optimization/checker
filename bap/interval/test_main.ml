open Core
open OUnit2

let suites = [Test_wrapping_interval.suite]

let () =
  let test = test_list suites in
  run_test_tt_main test
