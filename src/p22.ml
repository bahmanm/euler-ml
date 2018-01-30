(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries


let to_words s =
  s
  |> String.split_on_char ','
  |> List.map (fun s ->
      s |> String.trim % String.lchop % String.rchop
    )
  |> List.filter (fun s -> s <> "")

let score w =
  let a = (Char.code 'A') - 1 in
  w
  |> String.to_list
  |> List.map (fun c -> (Char.code c) - a)
  |> List.sum

let solve s =
  s
  |> to_words
  |> List.sort String.compare
  |> List.mapi (fun i s -> (i+1) * (score s))
  |> List.fold_left
    (fun acc e -> Int64.add acc (Int64.of_int e))
    Int64.zero
