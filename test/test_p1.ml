(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let solution _ =
  let printer = string_of_int in

  let data = 3 in
  let expected = 0 in
  let actual = data |> P1.solution in
  assert_equal ~printer expected actual;

  let data = 6 in
  let expected = 8 in
  let actual = data |> P1.solution in
  assert_equal ~printer expected actual;


  let data = 10 in
  let expected = 23 in
  let actual = data |> P1.solution in
  assert_equal ~printer expected actual;


  let data = 20 in
  let expected = 78 in
  let actual = data |> P1.solution in
  assert_equal ~printer expected actual

let suite_p1 =
  "P1 Suite" >:::
  ["solution" >:: solution]

(******************************************************************************)
let run _ =
  run_test_tt_main suite_p1
