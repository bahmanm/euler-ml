(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

let test_solve _ =
  assert_equal
    (P54.solve "")
    1

let p54_suite =
  "P54 Suite" >:::
  ["solve" >:: test_solve]
