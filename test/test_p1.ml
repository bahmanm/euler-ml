(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let test_solution _ =
  assert_equal 0 (P1.solution 3);
  assert_equal 8 (P1.solution 6);
  assert_equal 23 (P1.solution 10);
  assert_equal 78 (P1.solution 20)

let p1_suite =
  "P1 Suite" >:::
  ["solution" >:: test_solution]
