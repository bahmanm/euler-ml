(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries

let fibo_enum max =
  (fun () ->
     let second_last_term = ref 0 in
     let last_term = ref 1 in
     (fun () ->
        match !last_term + !second_last_term with
        | n when n >= max -> raise Enum.No_more_elements
        | n ->
          second_last_term := !last_term;
          last_term := n;
          n
     )
  )() |> Enum.from

let solve n =
  fibo_enum n |>
  Enum.fold
    (fun acc e -> if e mod 2 = 0 then acc + e else acc)
    0
