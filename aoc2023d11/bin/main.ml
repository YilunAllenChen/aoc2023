open Base
open Stdio

type ob = Galaxy | Space [@@deriving sexp]
type starmap = ob array array [@@deriving sexp]

let sm_to_str sm = sm |> [%sexp_of: starmap] |> Sexp.to_string_hum
let coord_to_str coord = coord |> [%sexp_of: int * int] |> Sexp.to_string_hum

let print_coords coords =
  List.map ~f:coord_to_str coords |> List.iter ~f:(printf "galaxy coord: %s\n")

let char_to_ob = function
  | '#' -> Galaxy
  | '.' -> Space
  | _ -> failwith "space garbage 😠"

let ob_to_char = function Galaxy -> '#' | Space -> '.'

let universe_to_text_map uni =
  let row_to_char line =
    Array.map line ~f:(fun ob -> ob_to_char ob)
    |> List.of_array |> String.of_list
  in
  Array.iter uni ~f:(fun row -> row_to_char row |> printf "%s\n")

let insert_array arr index elem =
  if index < 0 || index > Array.length arr then failwith "Index out of bounds";
  Array.concat
    [
      Array.sub arr ~pos:0 ~len:index;
      [| elem |];
      Array.sub arr ~pos:index ~len:(Array.length arr - index);
    ]

let distances coords =
  let dist (x1, y1) (x2, y2) = Int.abs (x2 - x1) + Int.abs (y2 - y1) in
  let rec calc_rest acc to_calc =
    match to_calc with
    | [] -> acc
    | here :: rest ->
        let all_dists =
          List.map rest ~f:(fun there -> (here, there, dist here there))
        in
        calc_rest (acc @ all_dists) rest
  in
  calc_rest [] coords

let distances_old coords empty_cols empty_rows mult =
  let dist (y1, x1) (y2, x2) =
    let left, right = if x1 < x2 then (x1, x2) else (x2, x1) in
    let hori_addened =
      List.count empty_cols ~f:(fun ele -> ele > left && ele < right) * mult
    in
    let up, down = if y1 < y2 then (y1, y2) else (y2, y1) in
    let vert_addened =
      List.count empty_rows ~f:(fun ele -> ele < down && ele > up) * mult
    in
    Int.abs (x2 - x1) + Int.abs (y2 - y1) + hori_addened + vert_addened
  in

  let rec calc_rest acc to_calc =
    match to_calc with
    | [] -> acc
    | here :: rest ->
        let all_dists =
          List.map rest ~f:(fun there -> (here, there, dist here there))
        in
        calc_rest (acc @ all_dists) rest
  in
  calc_rest [] coords

let is_all_space line = Array.for_all line ~f:(fun ele -> phys_equal ele Space)

let get_empty_rows_and_cols universe =
  let empty_row_nums =
    Array.filter_mapi universe ~f:(fun idx row ->
        if is_all_space row then Some idx else None)
    |> Array.to_list
  in
  let rec collect_empty_cols uni curr_col_num acc =
    if curr_col_num >= Array.length uni then acc
    else
      let this_col = Array.map uni ~f:(fun row -> row.(curr_col_num)) in
      if is_all_space this_col then
        collect_empty_cols uni (curr_col_num + 1) (curr_col_num :: acc)
      else collect_empty_cols uni (curr_col_num + 1) acc
  in
  let empty_col_nums = collect_empty_cols universe 0 [] in
  (* empty_col_nums |> [%sexp_of: int list] |> Sexp.to_string_hum |> print_endline; *)
  (* empty_row_nums |> [%sexp_of: int list] |> Sexp.to_string_hum |> print_endline; *)
  (empty_row_nums, empty_col_nums)

let expand_universe uni =
  let rec expand_horiontally curr_uni curr_col_num =
    if curr_col_num >= Array.length curr_uni.(0) then curr_uni
    else
      let this_col = Array.map curr_uni ~f:(fun row -> row.(curr_col_num)) in
      if is_all_space this_col then
        let new_uni =
          Array.map curr_uni ~f:(fun line ->
              insert_array line curr_col_num Space)
        in
        expand_horiontally new_uni (curr_col_num + 2)
      else expand_horiontally curr_uni (curr_col_num + 1)
  in
  let intermediate = expand_horiontally uni 0 in

  let new_row =
    let mtx =
      Array.make_matrix ~dimy:(Array.length intermediate.(0)) ~dimx:1 Space
    in
    mtx.(0)
  in

  let rec expand_vertically curr_uni curr_row_num =
    if curr_row_num >= Array.length curr_uni then curr_uni
    else
      let this_row = curr_uni.(curr_row_num) in
      if is_all_space this_row then
        let new_uni = insert_array curr_uni curr_row_num new_row in
        expand_vertically new_uni (curr_row_num + 2)
      else expand_vertically curr_uni (curr_row_num + 1)
  in
  expand_vertically intermediate 0

let parse_universe lines =
  let parse_line line = line |> String.to_array |> Array.map ~f:char_to_ob in
  List.map ~f:parse_line lines |> Array.of_list

let get_galaxies universe =
  let galaxy_from_row =
    Array.filter_mapi ~f:(fun idx ele ->
        if phys_equal ele Galaxy then Some idx else None)
  in
  universe
  |> Array.map ~f:galaxy_from_row
  |> Array.filter_mapi ~f:(fun idx row ->
         if Array.length row > 0 then Some (idx, row) else None)
  |> Array.fold ~init:[] ~f:(fun acc (row, cols) ->
         let exploded = Array.map cols ~f:(fun col -> (row, col)) in
         acc @ List.of_array exploded)

let () =
  ignore sm_to_str;
  print_endline "";
  let universe = In_channel.read_lines "data" |> parse_universe in
  universe |> universe_to_text_map;

  (* pt 1 *)
  let galaxy_coords = universe |> expand_universe |> get_galaxies in
  galaxy_coords |> print_coords;
  galaxy_coords |> distances
  |> List.fold ~init:0 ~f:(fun acc (_, _, dist) -> acc + dist)
  |> printf "pt1: %d\n";

  let raw_mult = 1_000_000 in
  let galaxy_coords = universe |> get_galaxies in
  let empty_rows, empty_cols = get_empty_rows_and_cols universe in
  let dist2 =
    distances_old galaxy_coords empty_cols empty_rows (raw_mult - 1)
  in

  (* dist2 |> [%sexp_of: ((int * int) * (int * int) * int) list] *)
  (* |> Sexp.to_string_hum |> print_endline; *)

  dist2
  |> List.fold ~init:0 ~f:(fun acc (_, _, dist) -> acc + dist)
  |> printf "pt2: %d\n"
