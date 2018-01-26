(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

(*TODO remove 0 from the beginning *)
let rec gen_numbers digits len =
  if len = 0 then
    [0]
  else match digits with
    | [] -> [0]
    | _ ->
      digits
      |> List.map
        (fun e ->
           let digits = e |> List.remove digits in
           let generated = gen_numbers digits (len - 1) in
           List.map
             (fun n -> n + e * (Int.pow 10 (len - 1)))
             generated
        )
      |> List.flatten

let is_prime n =
  Enum.range ~until:(n / 2) 2
  |> Enum.for_all (fun d -> n mod d <> 0)

let compare' x y =
  y - x

let rec solve = function
  | 0 -> -1
  | n ->
    let digits = [1;2;3;4;5;6;7;8;9] |> List.take n in
    (gen_numbers digits n)
    |> List.sort compare'
    |> List.find_opt is_prime |> function
    | Some x -> x
    | None -> solve (n - 1)
