(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
(* https://projecteuler.net/problem=2 *)

let solution limit =
  let rec solution' t1 t2 result =
    match t1 + t2 with
    | t3 when t3 <= limit -> begin
        match (t3 mod 2 = 0) with
        | true -> solution' t2 t3 (result + t3)
        | false -> solution' t2 t3 result
    end
    | _ -> result
  in
  solution' 1 1 0
