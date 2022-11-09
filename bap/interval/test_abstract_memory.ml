open Core
open Bap.Std
open Common
open OUnit2

module WI = Wrapping_interval
module T = Checker_taint.Analysis
module ProdWIT = DomainProduct(WI)(T)

module AM = Abstract_memory

module PtrWI = AM.Pointer(WI)
module Mem = AM.Make(WI)

module PtrWIT = AM.Pointer(ProdWIT)
module MemWIT = AM.Make(ProdWIT)

let assert_wi_equal = OUnit2.assert_equal ~cmp:WI.equal ~printer:WI.to_string

let get_intvl : ProdWIT.t -> WI.t =
  match ProdWIT.get WI.key with
  | Some f -> f
  | None -> failwith "Couldn't extract interval information out of product domain"

let get_taint : ProdWIT.t -> T.t =
  match ProdWIT.get T.key with
  | Some f -> f
  | None -> failwith "Couldn't extract taint information out of product domain"

let test_add_and_query_pointer _ =
  let name = "RSP" in
  let offs = WI.of_int 8000 in
  let mem = Mem.set_ptr Mem.empty
              ~name
              ~region:AM.Region.Stack
              ~offs
              ~width:`r64
  in
  
  let is_pointer = Mem.is_pointer ~name mem in
  let () = assert_bool "pointer not pointer" is_pointer in
    
  let maybe_offs = Mem.get_offset ~name mem in
  match maybe_offs with
  | Some i -> assert_wi_equal offs i
  | None -> assert_failure "offs not offs"

let test_set_rsp _ =
  let mem = Mem.empty in
  let is_pointer = Mem.is_pointer ~name:"RSP" mem in
  assert_bool "rsp should not be ptr" (not is_pointer);
  
  let mem = Mem.set_rsp 8000 Mem.empty in
  
  let is_pointer = Mem.is_pointer ~name:"RSP" mem in
  assert_bool "rsp should be ptr" is_pointer;
    
  let maybe_offs = Mem.get_offset ~name:"RSP" mem in
  let expected_offs = WI.of_int ~width:64 8000 in
  match maybe_offs with
  | Some i -> assert_wi_equal expected_offs i
  | None -> assert_failure "offs not offs"

let test_set_rbp _ =
  let mem = Mem.empty in
  let is_pointer = Mem.is_pointer ~name:"RBP" mem in
  assert_bool "rbp should not be ptr" (not is_pointer);
  
  let mem = Mem.set_rbp 8000 Mem.empty in
  
  let is_pointer = Mem.is_pointer ~name:"RBP" mem in
  assert_bool "rbp should be ptr" is_pointer;
    
  let maybe_offs = Mem.get_offset ~name:"RBP" mem in
  let expected_offs = WI.of_int ~width:64 8000 in
  match maybe_offs with
  | Some i -> assert_wi_equal expected_offs i
  | None -> assert_failure "offs not offs"

let test_store_rsp _ =
  assert_bool "todo" true

let test_load_rsp _ =
  assert_bool "todo" true

let suite = "Test_abstract_memory test suite" >:::
              ["test make pointer" >:: test_add_and_query_pointer;
               "test set_rsp works" >:: test_set_rsp;
               "test set_rbp works" >:: test_set_rbp;
               "test store to rsp works" >:: test_load_rsp;
               "test store to rbp works" >:: test_load_rsp]
