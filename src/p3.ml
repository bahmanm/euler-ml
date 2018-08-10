(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(* https://projecteuler.net/problem=3 *)

let solution n =
  let rec largest_prime_factor n f =
    match n = f with
    | true -> n
    | false -> match n mod f with
      | 0 -> largest_prime_factor (n / f) f
      | _ -> largest_prime_factor n (f + 1)
  in
  largest_prime_factor n 2
