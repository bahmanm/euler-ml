(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

module Set = BatSet.Int

let prime_factors n =
  let rec _prime_factors n f factors =
    if (n = f) then
      factors |> Set.add f
    else if (n mod f = 0) then
      factors |> Set.add f |>
      _prime_factors (n / f) f
    else
      factors |> _prime_factors n (f + 1)
  in
  Set.empty |> _prime_factors n 2

let solve n =
  let rec solve' result m =
    if (result |> List.length) = n then
      result |> List.last
    else begin
      if (m |> prime_factors |> Set.cardinal) = n then
        solve' (m :: result) (m + 1)
      else
        solve' [] (m + 1)
    end
  in
  solve' [] 2
