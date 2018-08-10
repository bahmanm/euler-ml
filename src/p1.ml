(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(* https://projecteuler.net/problem=1 *)

let solution n =
  let rec solution' x result =
    match x >= n with
    | true -> result
    | false -> begin
        let result = match (x mod 3 = 0) || (x mod 5 = 0) with
          | true -> result + x
          | false -> result
        in
        solution' (x + 1) result
      end
  in
  solution' 3 0
