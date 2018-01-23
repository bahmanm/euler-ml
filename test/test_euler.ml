(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

let dummy_test _ =
  assert_equal 1 1

let euler_suite =
  "Euler Test Suite" >:::
  ["Dummy test" >:: dummy_test]

let _ =
  run_test_tt_main euler_suite
