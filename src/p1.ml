(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

let sum_0n n =
  (n * (n + 1)) / 2

let sum_multiples_3_5 n =
  let n1 = n - 1 in
  let sum_mult_3 = 3 * (sum_0n (n1 / 3)) in
  let sum_mult_5 = 5 * (sum_0n (n1 / 5)) in
  let sum_mult_15 = 15 * (sum_0n (n1 /15)) in
  sum_mult_3 + sum_mult_5 - sum_mult_15

let solve n =
  sum_multiples_3_5 n
