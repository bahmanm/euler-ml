(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries
open Big_int

let two = 2 |> of_int

let factorial n =
  let rec factorial' n result =
    if equal n one then
      result
    else
      factorial' (sub n one) (mul n result)
  in
  factorial' n one

let solve n =
  let nn = n |> of_int in
  let numerator = factorial (mul nn two) in
  let denominator = pow (factorial nn) two in
  div numerator denominator
