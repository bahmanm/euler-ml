(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let solution _ =
  let printer = string_of_int in

  let data = 11 in
  let expected = 11 in
  let actual = data |> P3.solution in
  assert_equal ~printer expected actual;

  let data = 15 in
  let expected = 5 in
  let actual = data |> P3.solution in
  assert_equal ~printer expected actual;

  let data = 845 in
  let expected = 13 in
  let actual = data |> P3.solution in
  assert_equal ~printer expected actual;

  let data = 110 in
  let expected = 11 in
  let actual = data |> P3.solution in
  assert_equal ~printer expected actual

let suite_p3 =
  "P3 Suite" >:::
  ["solution" >:: solution]

(******************************************************************************)
let run _ =
  run_test_tt_main suite_p3
