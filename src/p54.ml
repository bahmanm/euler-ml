(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
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
module Hand = struct
  type card_t = int * int
  type t = card_t * card_t * card_t * card_t * card_t

  let card_of_string s =
    let clist = s |> String.to_list in
    let suit = (List.at clist 1) |> function
      | 'S' -> 1
      | 'D' -> 2
      | 'C' -> 3
      | 'H' -> 4
      | _ -> invalid_arg "card_of_string"
    in
    let value = (List.at clist 0) |> function
      | c when c >= '2' && c <= '9' ->
        (c |> Char.code) - ('0' |> Char.code)
      | 'T' -> 10
      | 'J' -> 11
      | 'Q' -> 12
      | 'K' -> 13
      | 'A' -> 14
      | _ -> invalid_arg "card_of_string"
    in
    (value, suit)

  let of_string =
    (String.split_on_char ' ') %> function
      | [c1; c2; c3; c4; c5] ->
        (c1 |> card_of_string,
         c2 |> card_of_string,
         c3 |> card_of_string,
         c4 |> card_of_string,
         c5 |> card_of_string)
      | _ -> invalid_arg "of_string"

  let pack_to_int = function
    | [v1; v2; v3; v4; v5] ->
      (v5 lsl 16) + (v4 lsl 12) + (v3 lsl 8) + (v2 lsl 4) + v1
    | _ -> invalid_arg "cards_to_int"

  let value ((v1, s1), (v2, s2), (v3, s3), (v4, s4), (v5, s5)) =
    let values = [v1; v2; v3; v4; v5] |> List.sort (-) in
    let smallest = values |> List.first in
    let nvalues = values |> List.map (fun v -> v - smallest) in
    let gvalues = nvalues
                  |> List.group Int.compare
                  |> List.map List.length
                  |> List.sort (-)
    in
    let rank =
      match s1 = s2 && s2 = s3 && s3 = s4 && s4 = s5 with
      | true -> begin
          match nvalues with
          | [0; 1; 2; 3; 4] -> 0x800000 (* straight flush *)
          | _ -> 0x500000 (* flush *)
        end
      | false -> begin
          match gvalues with
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
          | _ -> invalid_arg "value"
        end
    in
    rank + (values |> pack_to_int)

end
