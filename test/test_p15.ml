(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

let test_solve _ =
  assert_equal
    (Big_int.equal (P15.solve 2) (6 |> Big_int.of_int))
    true;
  assert_equal
    (Big_int.equal (P15.solve 3) (20 |> Big_int.of_int))
    true;
  assert_equal
    (Big_int.equal (P15.solve 20) (137846528820 |> Big_int.of_int))
    true

let p15_suite =
  "P15 Suite" >:::
  ["solve" >:: test_solve]
