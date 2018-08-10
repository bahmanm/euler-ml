(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2

let solution _ =
  let printer = string_of_int in

  let data = 10 in
  let expected = 10 in
  let actual = data |> P2.solution in
  assert_equal ~printer expected actual;

  let data = 100 in
  let expected = 44 in
  let actual = data |> P2.solution in
  assert_equal ~printer expected actual

let suite_p2 =
  "P2 Suite" >:::
  ["solution" >:: solution]

(******************************************************************************)
let run _ =
  run_test_tt_main suite_p2
