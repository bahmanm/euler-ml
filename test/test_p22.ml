(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

let test_solve _ =
  assert_equal
    (P22.solve "\"A\"")
    Int64.one;

  assert_equal
    (P22.solve "\"MARY\",\"PATRICIA\",\"LINDA\",\"BARBARA\",\"ELIZABETH\"")
    (952 |> Int64.of_int);

  assert_equal
    (File.with_file_in
       "res/p022_names.txt"
       (fun i -> IO.read_all i)
     |> P22.solve)
    (871198282 |> Int64.of_int)

let p22_suite =
  "P22 Suite" >:::
  ["solve" >:: test_solve]
