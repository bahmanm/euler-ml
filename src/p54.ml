(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(* https://projecteuler.net/problem=54 *)
open Batteries

(******************************************************************************)
module type HAND = sig
  (** type used to represent a hand of poker *)
  type t

  (** [of_string s] converts [s] to a [t] *)
  val of_string: string -> t

  (** [compare hand1 hand2] compares [hand1] and [hand2] *)
  val compare: t -> t -> int
end

(******************************************************************************)
(** Private implementation module *)
module HandImpl = struct
  type card_t = int * int
  type t = card_t * card_t * card_t * card_t * card_t

  let card_of_string s =
    match s |> String.explode with
    | [valuec; suitc] ->
      let value = valuec |> function
        | c when c >= '2' && c <= '9' ->
          (c |> Char.code) - ('0' |> Char.code)
        | 'T' -> 10
        | 'J' -> 11
        | 'Q' -> 12
        | 'K' -> 13
        | 'A' -> 14
        | _ -> invalid_arg("invalid value")
      in
      let suit = suitc |> function
        | 'S' -> 1
        | 'D' -> 2
        | 'C' -> 3
        | 'H' -> 4
        | _ -> invalid_arg("invalid suit")
      in
      (value, suit)
    | _ -> invalid_arg("invalid input")

  let of_string =
    (String.split_on_char ' ') %> function
      | [c1s; c2s; c3s; c4s; c5s] ->
        (c1s |> card_of_string,
         c2s |> card_of_string,
         c3s |> card_of_string,
         c4s |> card_of_string,
         c5s |> card_of_string)
      | _ -> invalid_arg("invalid input")

  let pack_to_int = function
    | [v1; v2; v3; v4; v5] ->
      (v5 lsl 16) + (v4 lsl 12) + (v3 lsl 8) + (v2 lsl 4) + v1
    | _ -> invalid_arg("invalid values")

  let sort_by_count_and_value =
    List.group compare
    %> List.sort begin fun g1 g2 ->
      let len1 = List.length g1 in
      let len2 = List.length g2 in
      let e1 = List.first g1 in
      let e2 = List.first g2 in
      match len1 = len2 with
      | true -> compare e1 e2
      | false -> compare len1 len2 end
    %> List.flatten

  let group_values =
    List.group compare %> List.map List.length %> List.sort compare

  let normalise_values values =
    let values = values |> List.sort compare in
    let smallest = values |> List.first in
    values |> List.map (fun v -> v - smallest)

  let value ((v1, s1), (v2, s2), (v3, s3), (v4, s4), (v5, s5)) =
    let values = [v1; v2; v3; v4; v5] in
    let nvalues = values |> normalise_values in
    let rank = match s1 = s2 && s2 = s3 && s3 = s4 && s4 = s5 with
    | true -> begin
        match nvalues  with
        | [0; 1; 2; 3; 4] -> 0x800000 (* straight flush *)
        | _ -> 0x500000 (* flush *)
      end
    | false -> begin
        match values |> group_values with
        | [1; 4] -> 0x700000 (* four of a kind *)
        | [2; 3] -> 0x600000 (* full house*)
        | [1; 1; 3] -> 0x300000 (* three of a kind *)
        | [1; 2; 2] -> 0x200000 (* two pairs *)
        | [1; 1; 1; 2] -> 0x100000 (* one pair *)
        | [1; 1; 1; 1; 1] -> begin
            match nvalues with
            | [0; 1; 2; 3; 4] -> 0x400000 (* straight *)
            | _ -> 0x00000
          end
        | _ -> invalid_arg("unknown hand")
      end
    in
    let value = values |> sort_by_count_and_value |> pack_to_int in
    value + rank

  let compare h1 h2 =
    let v1 = h1 |> value in
    let v2 = h2 |> value in
    compare v1 v2
end

module Hand : HAND = HandImpl

(******************************************************************************)
module type SCORE = sig
  (** represents the score of a hand;  *)
  type t = P1 | P2 | Draw | Invalid

  (** [of_enum e] processes all poker hands in [e] and returns the respective
      scores. *)
  val of_enum : string Enum.t -> t Enum.t
end

(******************************************************************************)
(** Private implementation module *)
module ScoreImpl = struct
  type t = P1 | P2 | Draw | Invalid

  let of_string line =
    let h1s = String.sub line 0 14 in
    let h2s = String.sub line 15 14 in
    let h1 = h1s |> Hand.of_string in
    let h2 = h2s |> Hand.of_string in
    Hand.compare h1 h2

  let of_enum =
    Enum.map of_string
    %> Enum.map begin function
      | 0 -> Draw
      | i when i > 0 -> P1
      | i when i < 0 -> P2
      | _ -> Invalid
    end
end

module Score : SCORE = ScoreImpl

(******************************************************************************)
let solution path =
  path |? "./test/res/p054_poker.txt" |> File.lines_of
  |> Score.of_enum
  |> Enum.filter begin function | Score.P1 -> true | _ -> false end
  |> Enum.count
