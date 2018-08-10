(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(* https://projecteuler.net/problem=15 *)
open Batteries

module Big = Big_int
let one = Big.one
let two = 2 |> Big.of_int

let factorial n =
  let rec factorial' n result =
    match Big.equal n one with
    | true -> result
    | false -> factorial' (Big.sub n one) (Big.mul n result)
  in
  factorial' n one

let solution n =
  let n = n |> Big_int.of_int in
  let numerator = factorial (Big.mul n two) in
  let denominator = Big.pow (factorial n) two in
  Big.div numerator denominator
