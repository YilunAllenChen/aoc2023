(* open Base *)
open Core

type token = Digit of int | Gear of char | Space
type number = { value : int; row : int; col : int } [@@deriving sexp]
type coord = { row : int; col : int } [@@deriving sexp]
type gear = { coord : coord; value : char } [@@deriving sexp]
type located_part = { number : number; gears : gear list } [@@deriving sexp]

let parse_to_tokens line =
  let rec parse_line' acc = function
    | [] -> acc
    | '.' :: xs -> parse_line' (Space :: acc) xs
    | x :: xs when Char.is_digit x ->
        parse_line' (Digit (Char.get_digit_exn x) :: acc) xs
    | x :: xs -> parse_line' (Gear x :: acc) xs
  in
  parse_line' [] (String.to_list line)

let split_at_non_digit tokens =
  let rec split_at_non_digit_to_two_lists' acc = function
    | [] -> acc
    | Digit x :: xs -> split_at_non_digit_to_two_lists' (x :: acc) xs
    | _ :: _ -> acc
  in
  let digits = split_at_non_digit_to_two_lists' [] tokens in
  let rest =
    List.drop_while tokens ~f:(function Digit _ -> true | _ -> false)
  in
  (digits, rest)

let collect_consecutive_digits str_line idx =
  let line = parse_to_tokens str_line in
  let line_length = List.length line in
  let rec keep_collect_digits curr_pos acc = function
    | [] -> acc
    | Digit x :: xs ->
        let digits, rest = split_at_non_digit (Digit x :: xs) in
        let new_pos = curr_pos + List.length digits in
        keep_collect_digits new_pos ((curr_pos, digits) :: acc) rest
    | _ :: xs -> keep_collect_digits (curr_pos + 1) acc xs
  in
  let cols_and_digits = keep_collect_digits 0 [] line in
  let cols_and_digits_to_number cad =
    let col, digits = cad in
    let length = List.length digits in
    let actual_col = line_length - col - length in
    let value =
      List.fold digits ~init:0 ~f:(fun acc digit -> (acc * 10) + digit)
    in
    { col = actual_col; row = idx; value }
  in

  cols_and_digits |> List.map ~f:cols_and_digits_to_number

let coords_to_scan max_col max_row (number : number) =
  let col_end = number.col + String.length (Int.to_string number.value) - 1 in
  let left = { row = number.row; col = number.col - 1 } in
  let right = { row = number.row; col = col_end + 1 } in
  let updowns =
    List.range (number.col - 1) (col_end + 2)
    |> List.map ~f:(fun col ->
           [ { row = number.row - 1; col }; { row = number.row + 1; col } ])
  in
  left :: right :: List.concat updowns
  |> List.filter ~f:(fun { row; col } ->
         row >= 0 && row <= max_row && col >= 0 && col <= max_col)

let is_part data (coord : coord) =
  let line = List.nth_exn data coord.row in
  let char = String.get line coord.col in
  not (Char.is_digit char || phys_equal char '.' || phys_equal char '\n')

let () =
  let data = In_channel.read_lines "data" in
  let max_row = List.length data - 1 in

  let arr = List.to_array data in

  let find_all_geared_parts idx line =
    let max_col = String.length line - 1 in
    let coords_to_scan_for_part = coords_to_scan max_col max_row in
    let parts = collect_consecutive_digits line idx in
    parts
    |> List.map ~f:(fun part ->
           let coords = coords_to_scan_for_part part in
           let gears =
             List.filter coords ~f:(is_part data)
             |> List.map ~f:(fun coord ->
                    { coord; value = arr.(coord.row).[coord.col] })
           in
           { number = part; gears })
  in

  let all_parts = List.mapi ~f:find_all_geared_parts data |> List.concat in

  (* pt 1 *)
  all_parts
  |> List.filter ~f:(fun { gears; _ } -> List.length gears > 0)
  |> List.fold ~init:0 ~f:(fun acc part -> acc + part.number.value)
  |> printf "pt1: %d\n";

  (* pt 2 *)
  let all_star_gears =
    List.map all_parts ~f:(fun { gears; _ } -> gears)
    |> List.concat
    |> List.dedup_and_sort ~compare:(fun g1 g2 ->
           let { row = r1; col = c1 } = g1.coord in
           let { row = r2; col = c2 } = g2.coord in
           if r1 = r2 then Int.compare c1 c2 else Int.compare r1 r2)
    |> List.filter ~f:(fun { value; _ } -> phys_equal value '*')
  in
  let gear_ratio gear =
    let connected_parts =
      List.filter all_parts ~f:(fun { gears; _ } ->
          List.exists gears ~f:(fun g ->
              g.coord.col = gear.coord.col && g.coord.row = gear.coord.row))
    in

    if List.length connected_parts = 2 then
      List.fold connected_parts ~init:1 ~f:(fun acc { number; _ } ->
          acc * number.value)
    else 0
  in
  all_star_gears |> List.map ~f:gear_ratio |> List.fold ~init:0 ~f:( + )
  |> printf "pt2: %d\n"
