(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

let test_solve _ =
  let senum = [
    "5H 5C 6S 7S KD 2C 3S 8S 8D TD";
    "5D 8C 9S JS AC 2C 5C 7D 8S QH";
    "2D 9C AS AH AC 3D 6D 7D TD QD";
    "4D 6S 9H QH QC 3D 6D 7H QD QS";
    "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D"
  ] |> List.enum
  in
  assert_equal
    (P54.solve senum)
    1

let p54_suite =
  "P54 Suite" >:::
  ["solve" >:: test_solve]
