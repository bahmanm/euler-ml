(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries


let solve limit =
  let rec solve' term1 term2 result =
    match term1 + term2 with
    | term3 when term3 <= limit -> begin
      match (term3 mod 2) with
      | 0 -> solve' term2 term3 (result + term3)
      | _ -> solve' term2 term3 result
    end
    | _ -> result
  in
  solve' 1 2 2
