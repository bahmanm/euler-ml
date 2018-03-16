(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

module Value : sig
  (** Card values *)
  type t = | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9
           | VT | VJ | VQ | VK | VA

  (** [of_int i] converts [i] to card values. *)
  val of_int: int -> t

  (** [compare v1 v2] compares [v1] and [v2].*)
  val compare: t -> t -> int

  (** [is_succ v w] determines if [v] is the immediate successor
        of [w].*)
  val is_succ_of: t -> t -> bool
end = struct
  type t = | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9
           | VT | VJ | VQ | VK | VA

  let of_int = function
    | 2 -> V2 | 3 -> V3 | 4 -> V4 | 5 -> V5
    | 6 -> V6 | 7 -> V7 | 8 -> V8 | 9 -> V9
    | 10 -> VT | 11 -> VJ | 12 -> VQ
    | 13 -> VK | 14 -> VA
    | _ -> invalid_arg "invalid value"

  let to_int = function
    | V2 -> 2 | V3 -> 3 | V4 -> 4 | V5 -> 5
    | V6 -> 6 | V7 -> 7 | V8 -> 8 | V9 -> 9
    | VT -> 10 | VJ -> 11 | VQ -> 12
    | VK -> 13 | VA -> 14

  let compare v1 v2 =
    Int.compare (to_int v1) (to_int v2)

  let is_succ_of v w =
    match w with
    | VA -> false
    | _ -> (to_int v) = (to_int w) + 1
end

(*******************************************************************************)
(** Card suits *)
type suit_t = | SC | SH | SD | SS

(** Card *)
type card_t = Card of Value.t * suit_t

(** Hand of cards *)
type hand_t = Hand of card_t list

(*******************************************************************************)
module HandCompare : sig
  (** [compare h1 h2] compares [h1] and [h2]*)
  val compare: hand_t -> hand_t -> int
end = struct
  type rank_t =
    | RoyalFlush
    | StraightFlush
    | FourOfAKind
    | FullHouse
    | Flush
    | Straight
    | ThreeOfAKind
    | TwoPairs
    | OnePair
    | HighCard

  let to_int = function
    | RoyalFlush -> 1000
    | StraightFlush -> 900
    | FourOfAKind -> 800
    | FullHouse -> 700
    | Flush -> 600
    | Straight -> 500
    | ThreeOfAKind -> 400
    | TwoPairs -> 300
    | OnePair -> 200
    | HighCard -> 100

  let values h =
    h |> List.map (fun (v, _) -> v)

  let sort_values h =
    h |> values |> List.sort (fun v1 v2 -> Value.compare v2 v1)

  let suits h =
    h |> List.map (fun (_, s) -> s)

  let same_suit h =
    h |> suits |> List.unique |> List.length = 1

  (****************************************************************************)
  module RankingFuns : sig
    type t = (Value.t * suit_t) list -> (int * Value.t list) option

    val funs: t list
  end = struct
    type t = (Value.t * suit_t) list -> (int * Value.t list) option

    let highCard h =
      Some(to_int HighCard, h |> sort_values)

    let onePair h =
      match h |> sort_values |> List.group_consecutive (=) with
      | [[p1; p2]; [v1]; [v2]; [v3]]
      | [[v1]; [p1; p2]; [v2]; [v3]]
      | [[v1]; [v2]; [p1; p2]; [v3]]
      | [[v1]; [v2]; [v3]; [p1; p2]] ->
        Some(to_int OnePair, [p1; v1; v2; v3])
      | _ -> None

    let twoPairs h =
      match h |> sort_values |> List.group_consecutive (=) with
      | [[p1; p2]; [p'1; p'2]; [v]]
      | [[p1; p2]; [v]; [p'1; p'2]]
      | [[v]; [p1; p2]; [p'1; p'2]] ->
        Some(to_int TwoPairs, [p1; p'1; v])
      | _ -> None

    let threeOfAKind h =
      match h |> sort_values |> List.group_consecutive (=) with
      | [[t1; t2; t3]; [v1]; [v2]]
      | [[v1]; [t1; t2; t3]; [v2]]
      | [[v1]; [v2]; [t1; t2; t3]] ->
        Some(to_int ThreeOfAKind, [t1; v1; v2])
      | _ -> None

    let straight h =
      match h |> sort_values with
      | [v1; v2; v3; v4; v5] when
          (Value.is_succ_of v1 v2) && (Value.is_succ_of v2 v3) &&
          (Value.is_succ_of v3 v4) && (Value.is_succ_of v4 v5) ->
        Some(to_int Straight, [v5])
      | _ -> None

    let flush h =
      if h |> same_suit |> not then
        None
      else
        Some(to_int Flush, h |> sort_values)

    let fullHouse h =
      match h |> sort_values |> List.group_consecutive (=) with
      | [[t1; t2; t3]; [p1; p2]]
      | [[p1; p2]; [t1; t2; t3]] ->
        Some(to_int FullHouse, [t1; p1])
      | _ -> None

    let fourOfAKind h =
      match h |> sort_values |> List.group_consecutive (=) with
      | [[f1; f2; f3; f4]; [v]]
      | [[v]; [f1; f2; f3; f4]] ->
        Some(to_int FourOfAKind, [f1; v])
      | _ -> None

    let straigthFlush h =
      if h |> same_suit |> not then
        None
      else
        match straight h with
        | Some(r, vs) -> Some(to_int StraightFlush, vs)
        | None -> None

    let royalFlush h =
      match (straigthFlush h) with
      | Some(r, vs) ->
        if Value.VA = (List.first vs) then
          Some(to_int RoyalFlush, [])
        else
          None
      | None -> None

    let funs = [
      royalFlush; straigthFlush; fourOfAKind; fullHouse; flush;
      straight; threeOfAKind; twoPairs; onePair; highCard
    ]
  end

  (****************************************************************************)
  let compare (Hand h1) (Hand h2) =
    let to_tuple (Card(v, s)) = (v, s) in
    let h1_cards = h1 |> List.map to_tuple in
    let h2_cards = h2 |> List.map to_tuple in
    let r1 = List.find_map (fun rf -> rf h1_cards) RankingFuns.funs in
    let r2 = List.find_map (fun rf -> rf h2_cards) RankingFuns.funs in
    Tuple2.compare
      ~cmp1:Int.compare
      ~cmp2:(List.compare Value.compare)
      r1 r2
end

(*******************************************************************************)
module HandLoader : sig
  (** [of_string s] converts a string into two hands. *)
  val of_string: string -> hand_t * hand_t
end = struct
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
      let value = v |> value_of_char |> Value.of_int in
      let suit = s |> suit_of_char in
      Card(value, suit)
    | _ -> invalid_arg "invalid string for card"

  let of_string s =
    let ss = s |> String.split_on_char ' ' |> List.map String.trim in
    if (ss |> List.length) <> 10 then
      invalid_arg "invalid string for hands"
    else
      let (ss1, ss2) = ss |> List.split_at 5 in
      let h1 = Hand(ss1 |> List.map card_of_string) in
      let h2 = Hand(ss2 |> List.map card_of_string) in
      (h1, h2)
end

(*******************************************************************************)
let solve' hands =
  hands
  |> Enum.map (fun s -> HandLoader.of_string s)
  |> Enum.map (fun (h1, h2) -> HandCompare.compare h1 h2)
  |> Enum.filter (fun i -> i = 1)
  |> Enum.count

let solve path =
  File.lines_of path |> solve'
