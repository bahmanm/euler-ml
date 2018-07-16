(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let test_solve _ =
  assert_equal
    (P54.solve "./test/res/p054_poker.txt")
    376

let p54_suite =
  "P54 Suite" >:::
  ["solve" >:: test_solve]
