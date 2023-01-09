open Core
open OUnit2

let suites = [Test_wrapping_interval.suite;
              Test_abstract_memory.suite]

let () =
  let test = test_list suites in
  run_test_tt_main test
