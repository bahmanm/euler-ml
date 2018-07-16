(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let test_solve _ =
  assert_equal
    (P1.solve 10)
    23;
  assert_equal
    (P1.solve 20)
    78;
  assert_equal
    (P1.solve 1000)
    233168

let p1_suite =
  "P1 Suite" >:::
  ["solve" >:: test_solve]
