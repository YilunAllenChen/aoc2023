open Base
open Core
open Stdio

type card = { id : int; winning_numbers : int list; have_numbers : int list }
[@@deriving sexp_of]

let parse_line line =
  match String.split ~on:':' line with
  | [ card_str; numbers_str ] ->
      let id =
        String.split ~on:' ' card_str |> List.last_exn |> Int.of_string
      in
      let all_numbers = String.split ~on:'|' numbers_str in

      let extract_numbers num_str =
        num_str |> String.split ~on:' '
        |> List.filter ~f:(fun x -> not (String.is_empty x))
        |> List.map ~f:(fun x -> Int.of_string x)
      in

      let winning_numbers = all_numbers |> List.hd_exn |> extract_numbers in
      let have_numbers = all_numbers |> List.last_exn |> extract_numbers in

      { id; winning_numbers; have_numbers }
  | _ -> failwith "bad line"

let score_card { id; winning_numbers; have_numbers } =
  ignore id;
  let rec accum_score acc = function
    | [] -> acc / 2
    | x :: xs ->
        accum_score
          (acc * if List.mem have_numbers x ~equal:phys_equal then 2 else 1)
          xs
  in
  accum_score 1 winning_numbers

let score_card_2 { id; winning_numbers; have_numbers } =
  ignore id;
  let rec accum_score acc = function
    | [] -> acc
    | x :: xs ->
        accum_score
          (acc + if List.mem have_numbers x ~equal:phys_equal then 1 else 0)
          xs
  in
  accum_score 0 winning_numbers

let rec count total_count lookup data =
  let data = List.sort ~compare:(fun a b -> Int.compare b.id a.id) data in
  let find_value_for key =
    match Map.find lookup key with None -> 0 | Some x -> x
  in
  match data with
  | [] -> total_count
  | x :: xs ->
      let num_new_cards = score_card_2 x in
      let added_card_ids = List.range (x.id + 1) (x.id + num_new_cards + 1) in
      let total_new_cards_added =
        added_card_ids |> List.map ~f:find_value_for
        |> List.fold ~init:1 ~f:( + )
      in
      let new_lookup =
        Map.add_exn lookup ~key:x.id ~data:total_new_cards_added
      in
      count (total_count + total_new_cards_added) new_lookup xs

let () =
  (* pt 1 *)
  let data = In_channel.read_lines "data" in

  data |> List.map ~f:parse_line |> List.map ~f:score_card
  |> List.fold ~init:0 ~f:( + ) |> printf "pt 1: %d\n";

  (* pt 2 *)
  data |> List.map ~f:parse_line
  |> count 0 (Map.empty (module Int))
  |> printf "pt 2: %d\n"
