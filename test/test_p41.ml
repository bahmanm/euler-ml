(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let test_solve _ =
  assert_equal
    (P41.solve 4)
    4231;
  assert_equal
    (P41.solve 7)
    7652413


let p41_suite =
  "P41 Suite" >:::
  ["solve" >:: test_solve]
