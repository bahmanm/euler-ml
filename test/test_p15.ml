(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

let solution _ =
  let printer = Big_int.to_string in
  let cmp = Big_int.equal in

  let data = 2 in
  let expected = 6 |> Big_int.of_int in
  let actual = data |> P15.solution in
  assert_equal ~cmp ~printer expected actual;

  let data = 3 in
  let expected = 20 |> Big_int.of_int in
  let actual = data |> P15.solution in
  assert_equal ~cmp ~printer expected actual

let suite_p15 =
  "P15 Suite" >:::
  ["solution" >:: solution]

(******************************************************************************)
let run _ =
  run_test_tt_main suite_p15
