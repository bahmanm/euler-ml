(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let _ =
  Test_p1.run();
  Test_p2.run();
  Test_p3.run();
  Test_p15.run();
  run_test_tt_main Test_p22.p22_suite;
  run_test_tt_main Test_p41.p41_suite;
  run_test_tt_main Test_p45.p45_suite;
  run_test_tt_main Test_p47.p47_suite;
  Test_p54.run()
