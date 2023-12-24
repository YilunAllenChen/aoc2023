open Base
open Stdio

type color = Red | Green | Blue [@@deriving sexp]

let to_color = function
  | "red" -> Red
  | "green" -> Green
  | "blue" -> Blue
  | _ -> failwith "bad data"

type draw = { color : color; count : int } [@@deriving sexp]
type game = { game_number : int; draws : draw list } [@@deriving sexp]

let extract_draws line =
  let extract_single_draw acc part =
    String.split part ~on:','
    |> List.map ~f:(fun s ->
           String.strip s |> String.split ~on:' ' |> function
           | [ count; color ] ->
               { color = to_color color; count = Int.of_string count }
           | _ -> failwith "bad data")
    |> List.rev_append acc
  in
  let parts = String.split line ~on:';' in
  List.fold parts ~init:[] ~f:extract_single_draw |> List.rev

let extract_game line =
  match String.split line ~on:':' with
  | [ game; draws ] ->
      let game_number =
        match String.split game ~on:' ' with
        | [ _; game_num ] -> game_num |> Int.of_string
        | _ -> failwith "bad data"
      in
      let draws = String.strip draws |> extract_draws in
      { game_number; draws }
  | _ -> failwith "bad data"

let max_count_for_each_color_in game =
  let max_count_for_color (color : color) =
    List.filter game.draws ~f:(fun draw -> phys_equal draw.color color)
    |> List.map ~f:(fun draw -> draw.count)
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  let red = max_count_for_color Red in
  let green = max_count_for_color Green in
  let blue = max_count_for_color Blue in
  (red, green, blue)

let counts_are_possible counts =
  let red, green, blue = counts in
  let red_is_possible = red <= 12 in
  let green_is_possible = green <= 13 in
  let blue_is_possible = blue <= 14 in
  red_is_possible && green_is_possible && blue_is_possible

let read_file_all_lines file =
  let channel = In_channel.create file in
  let rec read_all_lines' channel acc =
    match In_channel.input_line channel with
    | None -> acc
    | Some line -> read_all_lines' channel (line :: acc)
  in
  read_all_lines' channel [] |> List.rev

let () =
  (* pt 1 *)
  let all_lines = read_file_all_lines "data" in
  all_lines |> List.map ~f:extract_game
  |> List.filter ~f:(fun game ->
         max_count_for_each_color_in game |> counts_are_possible)
  |> List.map ~f:(fun game -> game.game_number)
  |> List.fold ~init:0 ~f:( + ) |> printf "%d\n";
  (* pt 2 *)
  let all_lines = read_file_all_lines "data" in
  all_lines |> List.map ~f:extract_game
  |> List.map ~f:max_count_for_each_color_in
  |> List.map ~f:(fun (red, green, blue) -> red * green * blue)
  |> List.fold ~init:0 ~f:( + ) |> printf "%d\n"
