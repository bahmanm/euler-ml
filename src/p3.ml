(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

let rec largest_prime_factor n f =
  if n = f then n
  else match n mod f with
    | 0 -> largest_prime_factor (n / f) f
    | _ -> largest_prime_factor n (f + 1)

let solve n =
  largest_prime_factor n 2
