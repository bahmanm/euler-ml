(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

type suit_t = SC | SH | SD | SS
type card_t = Card of int * suit_t
type hand_t = Hand of card_t list

(*******************************************************************************)
module HandRules = struct
  let sort_values =
    List.map (fun (Card(v, _)) -> v)
    %> List.sort Int.compare

  let card_suits =
    List.map (fun (Card(_, s)) -> s)

  let high_card (Hand(cards)) =
    cards |> sort_values |> List.last

  let is_one_pair (Hand(cards)) =
    let vals = cards |> sort_values in
    let groups = vals |> List.group_consecutive (=) in
    match (groups |> List.map List.length) with
    | [2; 1; 1; 1] | [1; 2; 1; 1] | [1; 1; 2; 1] | [1; 1; 1; 2] -> true
    | _ -> false

  let is_two_pairs (Hand(cards)) =
    let vals = cards |> sort_values in
    let groups = vals |> List.group_consecutive (=) in
    match (groups |> List.map List.length) with
    | [2; 2; 1] | [2; 1; 2] | [1; 2; 2] -> true
    | _ -> false

  let is_three_of_a_kind (Hand(cards)) =
    let vals = cards |> sort_values in
    let groups = vals |> List.group_consecutive (=) in
    groups |> List.map List.length |> List.max = 3

  let is_straight (Hand(cards)) =
    match (cards |> sort_values) with
    | [v1; v2; v3; v4; v5] ->
      v5 = (v4 + 1) && v4 = (v3 + 1) && v3 = (v2 + 1) && v2 = (v1 + 1)
    | _ -> false

  let is_flush (Hand(cards)) =
    match (cards |> card_suits |> List.unique) with
    | [s] -> true
    | _ -> false

  let is_full_house (Hand(cards)) =
    let vals = cards |> sort_values in
    let groups = vals |> List.group_consecutive (=) in
    match groups with
    | [g1; _] when (List.length g1 = 2) || (List.length g1 = 3) -> true
    | _ -> false

  let is_four_of_a_kind (Hand(cards)) =
    let vals = cards |> sort_values in
    let groups = vals |> List.group_consecutive (=) in
    match groups with
    | [g1; _] when (List.length g1 = 1) || (List.length g1 = 4) -> true
    | _ -> false

  let is_royal_flush (Hand(cards)) =
    match (cards |> card_suits |> List.unique) with
    | [s] -> begin
        match (cards |> sort_values) with
        | [10; 11; 12; 13; 14] -> true
        | _ -> false
      end
    | _ -> false

  let is_straight_flush (Hand(cards)) =
    match (cards |> card_suits |> List.unique) with
    | [s] -> begin
        match (cards |> sort_values) with
        | [10; 11; 12; 13; 14] -> true
        | _ -> false
      end
    | _ -> false
end

(*******************************************************************************)
module HandLoader = struct
  let value_of_char = function
    | c when c >= '1' && c <= '9' ->
      (c |> Char.code) - ('1' |> Char.code) + 1
    | 'T' -> 10
    | 'J' -> 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> invalid_arg "invalid char for card value"

  let suit_of_char = function
    | 'D' -> SD
    | 'C' -> SC
    | 'H' -> SH
    | 'S' -> SS
    | _ -> invalid_arg "invalid char for card suit"

  let card_of_string s =
    match (s |> String.to_list) with
    | [v; s] ->
      let value = v |> value_of_char in
      let suit = s |> suit_of_char in
      Card(value, suit)
    | _ -> invalid_arg "invalid string for card"

  let hands_of_string s =
    let ss = s |> String.split_on_char ' ' |> List.map String.trim in
    if (ss |> List.length) <> 10 then
      invalid_arg "invalid string for hands"
    else
      let (ss1, ss2) = ss |> List.split_at 5 in
      let h1 = Hand(ss1 |> List.map card_of_string) in
      let h2 = Hand(ss1 |> List.map card_of_string) in
      (h1, h2)
end

let hand_compare (h1, h2) =
  0

(*******************************************************************************)
let solve hands =
  0
