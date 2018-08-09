(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(* https://projecteuler.net/problem=1 *)
open Batteries

let solution n =
  (0--(n - 1))
  |> Enum.filter (fun x -> (x mod 3 = 0) || (x mod 5 = 0))
  |> Enum.sum
