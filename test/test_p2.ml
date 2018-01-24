(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

let test_solve _ =
  assert_equal
    (P2.solve 10)
    10;
  assert_equal
    (P2.solve 100)
    44;
  assert_equal
    (P2.solve 4_000_000)
    4613732

let p2_suite =
  "P2 Suite" >:::
  ["solve" >:: test_solve]
