(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

let _ =
  run_test_tt_main Test_p1.p1_suite;
  run_test_tt_main Test_p2.p2_suite;
  run_test_tt_main Test_p3.p3_suite;
  run_test_tt_main Test_p47.p47_suite
