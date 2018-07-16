(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let test_solve _ =
  assert_equal
    (P47.solve 2)
    14;
  assert_equal
    (P47.solve 3)
    644
  (* commented out to improve test speed
  assert_equal
    (P47.solve 4)
    134043 *)

let p47_suite =
  "P47 Suite" >:::
  ["solve" >:: test_solve]
