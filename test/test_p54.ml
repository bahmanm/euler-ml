(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries
open Result.Monad
open Printf

(******************************************************************************)
module HandImplSuite = struct
  let card_of_string _ =
    let data = "2S" in
    let expected = Ok(2, 1) in
    let actual = data |> P54.HandImpl.card_of_string in
    assert_equal expected actual;

    let data = "TC" in
    let expected = Ok(10, 3) in
    let actual = data |> P54.HandImpl.card_of_string in
    assert_equal expected actual;

    let data = "AH" in
    let expected = Ok(14, 4) in
    let actual = data |> P54.HandImpl.card_of_string in
    assert_equal expected actual

  (****************************)
  let of_string _ =
    let data = "8C TS KC 9H 4S" in
    let expected = Ok((8, 3), (10, 1), (13, 3), (9, 4), (4, 1)) in
    let actual = data |> P54.HandImpl.of_string in
    assert_equal expected actual;

    let data = "TS AS KC QH JS" in
    let expected = Ok((10, 1), (14, 1), (13, 3), (12, 4), (11, 1)) in
    let actual = data |> P54.HandImpl.of_string in
    assert_equal expected actual

  (****************************)
  let pack_to_int _ =
    let data = [1; 2; 3; 4; 5] in
    let expected = Ok(0x54321) in
    let actual = data |> P54.HandImpl.pack_to_int in
    assert_equal expected actual;

    let data = [4; 8; 9; 0xc; 0xc] in
    let expected = Ok(0xcc984) in
    let actual = data |> P54.HandImpl.pack_to_int in
    assert_equal expected actual

  (****************************)
  let sort_by_count_and_value _ =
    let printer = List.map string_of_int %> String.join "," in

    let data = [2; 5; 3; 4; 7] in
    let expected = [2; 3; 4; 5; 7] in
    let actual = data |> P54.HandImpl.sort_by_count_and_value in
    assert_equal ~printer expected actual;

    let data = [8; 8; 2; 2; 11] in
    let expected = [11; 2; 2; 8; 8] in
    let actual = data |> P54.HandImpl.sort_by_count_and_value in
    assert_equal ~printer expected actual;

    let data = [9; 9; 2; 2; 2] in
    let expected = [9; 9; 2; 2; 2] in
    let actual = data |> P54.HandImpl.sort_by_count_and_value in
    assert_equal ~printer expected actual

  (****************************)
  let value _ =
    let printer = function | Ok(r) -> sprintf "0x%x" r | Bad(s) -> s in

    let msg = "flush" in
    let data = ((2, 1), (5, 1), (3, 1), (4, 1), (7, 1)) in
    let expected = Ok(0x575432) in
    let actual = data |> P54.HandImpl.value in
    assert_equal ~msg ~printer expected actual;

    let msg = "straigh flush" in
    let data = ((2, 1), (3, 1), (4, 1), (5, 1), (6, 1)) in
    let expected = Ok(0x865432) in
    let actual = data |> P54.HandImpl.value in
    assert_equal ~msg ~printer expected actual;

    let msg = "straight" in
    let data = ((2, 1), (3, 2), (4, 4), (5, 1), (6, 3)) in
    let expected = Ok(0x465432) in
    let actual = data |> P54.HandImpl.value in
    assert_equal ~msg ~printer expected actual;

    let msg = "four of a kind" in
    let data = ((2, 1), (2, 2), (2, 3), (2, 4), (7, 1)) in
    let expected = Ok(0x722227) in
    let actual = data |> P54.HandImpl.value in
    assert_equal ~msg ~printer expected actual;

    let msg = "full house" in
    let data = ((2, 1), (2, 2), (2, 4), (4, 1), (4, 4)) in
    let expected = Ok(0x622244) in
    let actual = data |> P54.HandImpl.value in
    assert_equal ~msg ~printer expected actual;

    let msg = "three of a kind" in
    let data = ((2, 1), (2, 2), (2, 4), (4, 1), (6, 3)) in
    let expected = Ok(0x322264) in
    let actual = data |> P54.HandImpl.value in
    assert_equal ~msg ~printer expected actual;

    let msg = "two pairs" in
    let data = ((2, 1), (2, 2), (4, 4), (4, 1), (6, 3)) in
    let expected = Ok(0x244226) in
    let actual = data |> P54.HandImpl.value in
    assert_equal ~msg ~printer expected actual;

    let msg = "one pair" in
    let data = ((8, 3), (10, 1), (9, 3), (9, 4), (4, 1)) in
    let expected = Ok(0x199a84) in
    let actual = data |> P54.HandImpl.value in
    assert_equal ~msg ~printer expected actual;

    let msg = "high card" in
    let data = ((2, 1), (3, 2), (6, 4), (8, 1), (11, 3)) in
    let expected = Ok(0x0b8632) in
    let actual = data |> P54.HandImpl.value in
    assert_equal ~msg ~printer expected actual

  (****************************)
  let compare _ =
    let msg = "draw" in
    let expected = Ok(0) in
    let actual =
      "8C TS KC 9H 4S" |> P54.HandImpl.of_string
      >>= fun data1 -> "8C TS KC 9H 4S" |> P54.HandImpl.of_string
      >>= fun data2 -> P54.HandImpl.compare data1 data2
    in
    assert_equal ~msg expected actual;

    let msg = "loss" in
    let expected = Ok(-1) in
    let actual =
      "8C 8S KC 9H 4S" |> P54.HandImpl.of_string
      >>= fun data1 -> "8C TS 9C 9H 4S" |> P54.HandImpl.of_string
      >>= fun data2 -> P54.HandImpl.compare data1 data2
    in
    assert_equal ~msg expected actual;

    let msg = "win" in
    let expected = Ok(1) in
    let actual =
      "8C 8S 8D 8H 4S" |> P54.HandImpl.of_string
      >>= fun data1 -> "8C 8S 9C 9H 4S" |> P54.HandImpl.of_string
      >>= fun data2 -> P54.HandImpl.compare data1 data2
    in
    assert_equal ~msg expected actual;
end

let suite_HandImpl =
  "P54-HandImpl-Suite" >:::
  ["card_of_string" >:: HandImplSuite.card_of_string;
   "of_string" >:: HandImplSuite.of_string;
   "pack_to_int" >:: HandImplSuite.pack_to_int;
   "value" >:: HandImplSuite.value;
   "sort_by_group_and_value" >:: HandImplSuite.sort_by_count_and_value;
   "compare" >:: HandImplSuite.compare]

(******************************************************************************)
module ScoreImplSuite = struct
  let of_string _ =
    let printer = function | Ok(i) -> sprintf "%d" i | Bad(s) -> s in

    let msg = "P1 win" in
    let data = "5H KS 9C 7D 9H 8D 3S 5D 5C AH" in
    let expected = Ok(1) in
    let actual = data |> P54.ScoreImpl.of_string in
    assert_equal ~msg ~printer expected actual;

    let msg = "P1 loss" in
    let data = "2H 4S 5C 5S TC KC JC 6C TC 3C" in
    let expected = Ok(-1) in
    let actual = data |> P54.ScoreImpl.of_string in
    assert_equal ~msg ~printer expected actual;

    let msg = "draw" in
    let data = "2H 2S 5C 5S TC 2C 2D 5D 5H TS" in
    let expected = Ok(0) in
    let actual = data |> P54.ScoreImpl.of_string in
    assert_equal ~msg ~printer expected actual

  (****************************)
  let of_enum _ =
    let printer =
      Enum.map begin function
        | P54.ScoreImpl.P1 -> "P1"
        | P54.ScoreImpl.P2 -> "P2"
        | P54.ScoreImpl.Draw -> "Draw"
        | P54.ScoreImpl.Invalid -> "Invalid"
      end
      %> List.of_enum %> String.join ","
    in
    let cmp en1 en2 = (List.of_enum en1) = (List.of_enum en2) in

    let msg = "no lines" in
    let data = Enum.empty() in
    let actual = data |> P54.ScoreImpl.of_enum in
    let expected = Enum.empty() in
    assert_equal ~msg ~cmp ~printer expected actual;

    let msg = "only 1 line" in
    let data = "5H KS 9C 7D 9H 8D 3S 5D 5C AH" |> Enum.singleton in
    let actual = data |> P54.ScoreImpl.of_enum in
    let expected = P54.ScoreImpl.P1 |> Enum.singleton in
    assert_equal ~msg ~cmp ~printer expected actual;

    let msg = "many lines" in
    let data = ["5H KS 9C 7D 9H 8D 3S 5D 5C AH";
                "2H 4S 5C 5S TC KC JC 6C TC 3C";
                "2H 2S 5C 5S TC 2C 2D 5D 5H TS"]
               |> List.enum
    in
    let actual = data |> P54.ScoreImpl.of_enum in
    let expected = [P54.ScoreImpl.P1; P54.ScoreImpl.P2; P54.ScoreImpl.Draw]
                   |> List.enum
    in
    assert_equal ~msg ~cmp ~printer expected actual
end

let suite_ScoreImpl =
  "P54-ScoreImpl-Suite" >:::
  ["of_string" >:: ScoreImplSuite.of_string;
   "of_enum" >:: ScoreImplSuite.of_enum]

(******************************************************************************)
let run _ =
  run_test_tt_main suite_HandImpl;
  run_test_tt_main suite_ScoreImpl
