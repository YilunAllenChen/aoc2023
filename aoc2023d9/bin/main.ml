open Base
open Stdio

let parse_line line =
  line |> String.strip |> String.split ~on:' ' |> List.map ~f:Int.of_string

let build_pyramid series =
  let rec series_diff acc = function
    | [] -> acc |> List.rev
    | _ :: [] -> acc |> List.rev
    | x :: y :: xs -> series_diff ((y - x) :: acc) (y :: xs)
  in
  let rec continue acc series =
    match series with
    | [] -> acc
    | _ ->
        let diff = series_diff [] series in
        if List.exists ~f:(fun x -> not @@ equal x 0) diff then
          continue (diff :: acc) diff
        else acc
  in
  continue [ series ] series
(* |> List.map ~f:List.rev *)

let extend_pyramid pyramid =
  let rec extend acc addend rest =
    match rest with
    | [] -> acc
    | line :: rest ->
        let last = List.last_exn line in
        let new_addend = last + addend in
        let new_line = List.append line [ new_addend ] in
        extend (new_line :: acc) new_addend rest
  in
  extend [] 0 pyramid |> List.rev

let backfill_pyramid pyramid =
  let rec backfill acc addend rest =
    match rest with
    | [] -> acc
    | line :: rest ->
        let fst = List.hd_exn line in
        let new_addend = fst - addend in
        let new_line = new_addend :: line in
        backfill (new_line :: acc) new_addend rest
  in
  backfill [] 0 pyramid |> List.rev

let extract_corner pyramid = pyramid |> List.last_exn |> List.last_exn
let extract_corner_2 pyramid = pyramid |> List.last_exn |> List.hd_exn

let build_extend_and_extract line =
  line |> parse_line |> build_pyramid |> extend_pyramid |> extract_corner

let build_backfill_and_extract line =
  line |> parse_line |> build_pyramid |> backfill_pyramid |> extract_corner_2

let () =
  let data = In_channel.read_lines "data" in

  (* pt1 *)
  data
  |> List.map ~f:build_extend_and_extract
  |> List.fold ~init:0 ~f:( + ) |> printf "pt1: %d\n";

  (* pt2 *)
  data
  |> List.map ~f:build_backfill_and_extract
  |> List.fold ~init:0 ~f:( + ) |> printf "pt2: %d\n"
