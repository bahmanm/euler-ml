(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open OUnit2
open Batteries

module HandSuite = struct
  let card_of_string _ =
    let data = "2S" in
    let expected = Ok(2, 1) in
    let actual = data |> P54.Hand.card_of_string in
    assert_equal expected actual;

    let data = "TC" in
    let expected = Ok(10, 3) in
    let actual = data |> P54.Hand.card_of_string in
    assert_equal expected actual;

    let data = "AH" in
    let expected = Ok(14, 4) in
    let actual = data |> P54.Hand.card_of_string in
    assert_equal expected actual

  let of_string _ =
    let data = "8C TS KC 9H 4S" in
    let expected = Ok((8, 3), (10, 1), (13, 3), (9, 4), (4, 1)) in
    let actual = data |> P54.Hand.of_string in
    assert_equal expected actual;

    let data = "TS AS KC QH JS" in
    let expected = Ok((10, 1), (14, 1), (13, 3), (12, 4), (11, 1)) in
    let actual = data |> P54.Hand.of_string in
    assert_equal expected actual

  let pack_to_int _ =
    let data = [1; 2; 3; 4; 5] in
    let expected = Ok(0x54321) in
    let actual = data |> P54.Hand.pack_to_int in
    assert_equal expected actual;

    let data = [4; 8; 9; 0xc; 0xc] in
    let expected = Ok(0xcc984) in
    let actual = data |> P54.Hand.pack_to_int in
    assert_equal expected actual

  let value _ =
    let data = ((2, 1), (2, 1), (3, 1), (4, 1), (7, 1)) in
    let expected = Ok(0x574322) in
    let actual = data |> P54.Hand.value in
    assert_equal ~msg:"flush" expected actual;

    let data = ((2, 1), (3, 1), (4, 1), (5, 1), (6, 1)) in
    let expected = Ok(0x865432) in
    let actual = data |> P54.Hand.value in
    assert_equal ~msg:"straigh flush" expected actual;

    let data = ((2, 1), (3, 2), (4, 4), (5, 1), (6, 3)) in
    let expected = Ok(0x465432) in
    let actual = data |> P54.Hand.value in
    assert_equal ~msg:"straight" expected actual;

    let data = ((2, 1), (2, 2), (2, 3), (2, 4), (7, 1)) in
    let expected = Ok(0x772222) in
    let actual = data |> P54.Hand.value in
    assert_equal ~msg:"four of a kind" expected actual;

    let data = ((2, 1), (2, 2), (2, 4), (4, 1), (4, 4)) in
    let expected = Ok(0x644222) in
    let actual = data |> P54.Hand.value in
    assert_equal ~msg:"full house" expected actual;

    let data = ((2, 1), (2, 2), (2, 4), (4, 1), (6, 3)) in
    let expected = Ok(0x364222) in
    let actual = data |> P54.Hand.value in
    assert_equal ~msg:"three of a kind" expected actual;

    let data = ((2, 1), (2, 2), (4, 4), (4, 1), (6, 3)) in
    let expected = Ok(0x264422) in
    let actual = data |> P54.Hand.value in
    assert_equal ~msg:"two pairs" expected actual;

    let data = ((2, 1), (2, 2), (4, 4), (9, 1), (11, 3)) in
    let expected = Ok(0x1b9422) in
    let actual = data |> P54.Hand.value in
    assert_equal ~msg:"one pair" expected actual;

    let data = ((2, 1), (3, 2), (6, 4), (8, 1), (11, 3)) in
    let expected = Ok(0x0b8632) in
    let actual = data |> P54.Hand.value in
    assert_equal ~msg:"high card" expected actual;
end

let p54_suite =
  "P54-Hand-Suite" >:::
  ["card_of_string" >:: HandSuite.card_of_string;
   "of_string" >:: HandSuite.of_string;
   "pack_to_int" >:: HandSuite.pack_to_int;
   "value" >:: HandSuite.value]
