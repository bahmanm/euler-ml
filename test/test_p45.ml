(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

let test_solve _ =
  assert_equal
    (P45.solve 283)
    40755;
  assert_equal
    (P45.solve 286)
    1533776805


let p45_suite =
  "P45 Suite" >:::
  ["solve" >:: test_solve]
