(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let test_solve _ =
  assert_equal
    (P3.solve 11)
    11;
  assert_equal
    (P3.solve 13195)
    29;
  assert_equal
    (P3.solve 600851475143)
    6857

let p3_suite =
  "P3 Suite" >:::
  ["solve" >:: test_solve]
