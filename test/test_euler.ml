(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let _ =
  run_test_tt_main Test_p1.p1_suite;
  run_test_tt_main Test_p2.p2_suite;
  run_test_tt_main Test_p3.p3_suite;
  run_test_tt_main Test_p15.p15_suite;
  run_test_tt_main Test_p22.p22_suite;
  run_test_tt_main Test_p41.p41_suite;
  run_test_tt_main Test_p45.p45_suite;
  run_test_tt_main Test_p47.p47_suite;
  run_test_tt_main Test_p54.p54_suite
