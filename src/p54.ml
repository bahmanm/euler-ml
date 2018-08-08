(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries
open Result.Monad

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
    match s |> String.explode with
    | [valuec; suitc] ->
      begin match valuec with
        | c when c >= '2' && c <= '9' ->
          Ok((c |> Char.code) - ('0' |> Char.code))
        | 'T' -> Ok(10)
        | 'J' -> Ok(11)
        | 'Q' -> Ok(12)
        | 'K' -> Ok(13)
        | 'A' -> Ok(14)
        | _ -> Bad("invalid value")
      end
      >>= begin fun value ->
        match suitc with
        | 'S' -> Ok(value, 1)
        | 'D' -> Ok(value, 2)
        | 'C' -> Ok(value, 3)
        | 'H' -> Ok(value, 4)
        | _ -> Bad("invalid suit")
      end
    | _ -> Bad("invalid input")

  let of_string =
    (String.split_on_char ' ') %> function
      | [c1s; c2s; c3s; c4s; c5s] ->
        (c1s |> card_of_string)
        >>= fun c1 -> c2s |> card_of_string
        >>= fun c2 -> Ok(c1, c2)
        >>= fun (c1, c2) -> c3s |> card_of_string
        >>= fun c3 -> Ok(c1, c2, c3)
        >>= fun (c1, c2, c3) -> c4s |> card_of_string
        >>= fun c4 -> Ok(c1, c2, c3, c4)
        >>= fun (c1, c2, c3, c4) -> c5s |> card_of_string
        >>= fun c5 -> Ok(c1, c2, c3, c4, c5)
      | _ -> Bad("invalid input")

  let pack_to_int = function
    | [v1; v2; v3; v4; v5] ->
      let n = (v5 lsl 16) + (v4 lsl 12) + (v3 lsl 8) + (v2 lsl 4) + v1 in
      Ok(n)
    | _ -> Bad("cards_to_int")

  let value ((v1, s1), (v2, s2), (v3, s3), (v4, s4), (v5, s5)) =
    let values = [v1; v2; v3; v4; v5] |> List.sort (-) in
    let normalised_values = values |> List.map (fun v -> v - (List.at values 0)) in
    let grouped_values = normalised_values |> List.group Int.compare
                         |> List.map List.length |> List.sort (-)
    in
    let rank =
      match s1 = s2 && s2 = s3 && s3 = s4 && s4 = s5 with
      | true -> begin
          match normalised_values with
          | [0; 1; 2; 3; 4] -> 0x800000 (* straight flush *)
          | _ -> 0x500000 (* flush *)
        end
      | false -> begin
          match grouped_values with
          | [1; 4] -> 0x700000 (* four of a kind *)
          | [2; 3] -> 0x600000 (* full house*)
          | [1; 1; 3] -> 0x300000 (* three of a kind *)
          | [1; 2; 2] -> 0x200000 (* two pairs *)
          | [1; 1; 1; 2] -> 0x100000 (* one pair *)
          | [1; 1; 1; 1; 1] -> begin
              match normalised_values with
              | [0; 1; 2; 3; 4] -> 0x400000 (* straight *)
              | _ -> 0x00000
            end
          | _ -> invalid_arg "value"
        end
    in
    (values |> pack_to_int) >>= fun value -> Ok(value + rank)

end
