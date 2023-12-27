open Base
open Stdio

type game = { time : int; distance : int } [@@deriving sexp]

let strip_line line =
  match String.split line ~on:':' with
  | [ _; numbers_str ] ->
      String.split numbers_str ~on:' '
      |> List.filter ~f:(fun x -> String.length x > 0)
      |> List.map ~f:Int.of_string
  | _ -> failwith "bad data"

let strip_line_2 line =
  match String.split line ~on:':' with
  | [ _; numbers_str ] ->
      String.split numbers_str ~on:' '
      |> List.filter ~f:(fun x -> String.length x > 0)
      |> String.concat ~sep:"" |> Int.of_string
  | _ -> failwith "bad data"

let is_ok hold_t run_t distance = hold_t * run_t > distance

let count_ok_permutation_for_game game =
  let upper_bound = game.time / 2 in
  let rec find_lowest_possible curr_hold_t =
    if is_ok curr_hold_t (game.time - curr_hold_t) game.distance then
      curr_hold_t
    else find_lowest_possible (curr_hold_t + 1)
  in
  let lower_bound = find_lowest_possible 1 in
  let addend = match game.time % 2 = 0 with true -> -1 | false -> 0 in
  (2 * (upper_bound - lower_bound + 1)) + addend

let () =
  let data =
    In_channel.read_lines "data"
    |> List.filter ~f:(fun x -> String.length x > 0)
  in
  match data with
  | [ time; distance ] ->
      (* pt 1 *)
      let times = time |> strip_line in
      let distances = distance |> strip_line in
      let zipped = List.zip_exn times distances in
      zipped
      |> List.map ~f:(fun (t, d) -> { time = t; distance = d })
      |> List.map ~f:count_ok_permutation_for_game
      |> List.fold ~init:1 ~f:(fun acc x -> acc * x)
      |> printf "pt1: %d\n";

      (* pt 2 *)
      let time_2 = time |> strip_line_2 in
      let distance_2 = distance |> strip_line_2 in
      let game_2 = { time = time_2; distance = distance_2 } in
      count_ok_permutation_for_game game_2 |> printf "pt2: %d\n"
  | _ -> failwith "bad data"
