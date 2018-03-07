(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

type suit = SC | SH | SD | SS
type card = Card of int * suit
type hand = Hand of card list

let sort_values =
  List.map (fun (Card(v, _)) -> v)
  %> List.sort Int.compare

let card_suits =
  let int_of_suit = function | SC -> 1 | SH -> 2 | SD -> 2 | SS -> 3 in
  List.map (fun (Card(_, s)) -> s)
  %> List.sort (fun s1 s2 -> Int.compare (int_of_suit s1) (int_of_suit s2))

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


let solve hands =
  0
