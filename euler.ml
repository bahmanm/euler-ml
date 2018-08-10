(* Author: Bahman Movaqar <Bahman@BahmanM.com> *)
open Batteries
open BatPrintf


module Utils = struct
  let time f =
    let t1 = Sys.time() in
    let result = f() in
    let t2 = Sys.time() in
    let t = ((t2 -. t1) *. 1_000_000.) |> int_of_float in
    (result, t)

  let thousand_sep n =
    let rec f idx ll =
      match ll with
      | [] -> []
      | hd :: tl when idx mod 3 = 0 && (List.is_empty tl = false)->
        hd :: ',' :: (f (idx + 1) tl)
      | hd :: tl ->
        hd :: (f (idx + 1) tl)
    in
    n
    |> String.to_list |> List.rev
    |> f 1
    |> List.rev |> String.of_list
end

(*******************************************************************************)
module Printer = struct
  type problem_t = unit -> string

  let print_header () =
    printf "\n\n";
    printf " ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓ \n";
    printf " ┃                         PROJECT EULER ANSWERS                              ┃ \n";
    printf " ┣━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫ \n";
    printf " ┃ PROBLEM #  ┃                 ANSWER           ┃          TIME (µs)         ┃ \n";
    printf " ┣━━━━━━━━━━━━╇━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╇━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫ \n"


  let separator_line =
    sprintf " ┠────────────┼──────────────────────────────────┼────────────────────────────┨ \n"


  let print_last_line () =
    printf " ┗━━━━━━━━━━━━┷━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┷━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ \n"

  let result_line n result t =
    sprintf " ┃ %10s │ %32s │ %26s ┃ \n"
      (Utils.thousand_sep (string_of_int n))
      result
      (Utils.thousand_sep (string_of_int t))

  let rec print_problems = function
    | [] -> ()
    | p :: [] -> print_string (p()); printf "%!";
    | p :: tl -> begin
        print_string (p());
        print_string separator_line;
        printf "%!";
        print_problems tl
      end
end

(*******************************************************************************)
let problems : Printer.problem_t list =
  [
    (* 1 *)
    begin fun _ ->
      let (r, t) = Utils.time (fun _ -> P1.solution 1000) in
      Printer.result_line 1 (Utils.thousand_sep (string_of_int r)) t
    end;

    (* 2 *)
    begin fun _ ->
      let (r, t) = Utils.time (fun _ -> P2.solution 4_000_000) in
      Printer.result_line 2 (Utils.thousand_sep (string_of_int r)) t
    end;

    (* 3 *)
    begin fun _ ->
      let (r, t) = Utils.time (fun _ -> P3.solution 600_851_475_143) in
      Printer.result_line 3 (Utils.thousand_sep (string_of_int r)) t
    end;

    (* 15 *)
    begin fun _ ->
      let (r, t) = Utils.time (fun _ -> P15.solve 20) in
      Printer.result_line 15 (Utils.thousand_sep (Big_int.string_of_big_int r)) t
    end;

    (* 22 *)
    begin fun _ ->
      let s = begin
        File.with_file_in
          "./test/res/p022_names.txt"
          (fun i -> IO.read_all i)
      end in
      let (r, t) = Utils.time (fun _ -> P22.solve s) in
      Printer.result_line 22 (Utils.thousand_sep (Int64.to_string r)) t
    end;

    (* 41 *)
    begin fun _ ->
      let (r, t) = Utils.time (fun _ -> P41.solve 10) in
      Printer.result_line 41 (Utils.thousand_sep (string_of_int r)) t
    end;

    (* 45 *)
    begin fun _ ->
      let (r, t) = Utils.time (fun _ -> P45.solve 285) in
      Printer.result_line 45 (Utils.thousand_sep (string_of_int r)) t
    end;

    (* 54 *)
    begin fun _ ->
      let (r, t) = Utils.time (fun _ -> P54.solution None) in
      Printer.result_line 54 (Utils.thousand_sep (string_of_int r)) t
    end;
  ]

(*******************************************************************************)
let _ =
  Printer.print_header();
  problems |> Printer.print_problems;
  Printer.print_last_line()
