(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries


let pick_answer x1 x2 =
  if x1 < 0. && x2 < 0. then
    None
  else
  if x1 < 0. && (floor x2) = x2 then
    Some(x2 |> floor |> Float.to_int)
  else
  if x2 < 0. && (floor x1) = x1 then
    Some(x1 |> floor |> Float.to_int)
  else
    None

let reverse_pentagonal n =
  let delta2 = (0.25 +. (6. *. n)) in
  if delta2 < 0. then
    None
  else
    let delta = sqrt delta2 in
    let x1 = (0.5 +. delta) /. 3. in
    let x2 = (0.5 -. delta) /. 3. in
    pick_answer x1 x2

let reverse_hexagonal n =
  let delta2 = (1. +. 8. *. n) in
  if delta2 < 0. then
    None
  else
    let delta = sqrt delta2 in
    let x1 = 0.25 +. delta /. 4. in
    let x2 = 0.25 -. delta /. 4. in
    pick_answer x1 x2

let triangle n =
  (n * (n + 1)) / 2

let solve min =
  min
  |> Enum.range
  |> Enum.map (fun x -> (x, triangle x |> Float.of_int))
  |> Enum.find_map (fun (_, tx) ->
      (reverse_pentagonal tx, reverse_hexagonal tx) |> function
      | (Some(_), Some(_)) -> Some(tx |> Float.to_int)
      | _ -> None
    )
