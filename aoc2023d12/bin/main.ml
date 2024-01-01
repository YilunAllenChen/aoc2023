open Base
open Stdio

type loc = Oper | Dmgd | Unkn [@@deriving sexp_of]

type line = { locs : loc array; raw : string; stats : int array }
[@@deriving sexp_of]

let free_locs_in_line line =
  let dmged_recorded = Array.fold line.stats ~init:0 ~f:( + ) in
  let dmgd_seen =
    Array.length (Array.filter line.locs ~f:(fun x -> phys_equal x Dmgd))
  in
  let ops_seen =
    Array.length (Array.filter line.locs ~f:(fun x -> phys_equal x Oper))
  in
  let dmgd_to_place = dmged_recorded - dmgd_seen in
  let ops_to_place =
    Array.length line.locs - dmgd_seen - dmgd_to_place - ops_seen
  in
  (dmgd_to_place, ops_to_place)

let get_free_pos line =
  Array.filter_mapi line.locs ~f:(fun i x ->
      if phys_equal x Unkn then Some i else None)

let is_valid_line line =
  let pattern = Re2.of_string "(#)+" in
  let all_matches = Re2.find_all_exn pattern line.raw in
  all_matches |> List.map ~f:String.length |> Array.of_list
  |> Array.equal (fun a b -> a = b) line.stats

let char_to_loc = function
  | '.' -> Oper
  | '#' -> Dmgd
  | '?' -> Unkn
  | _ -> failwith "Invalid char"

let loc_to_char = function
  | Oper -> "."
  | Dmgd -> "#"
  | _ -> failwith "can't do that"

let parse_line line =
  match String.split line ~on:' ' with
  | [ locs; stats ] ->
      let stats =
        stats |> String.split ~on:',' |> Array.of_list
        |> Array.map ~f:Int.of_string
      in
      let locs =
        locs |> String.to_list |> List.map ~f:char_to_loc |> Array.of_list
      in
      { locs; stats; raw = line }
  | _ -> failwith "Invalid line"

let rec permutations a_c b_c =
  match (a_c, b_c) with
  | 0, 0 -> [ [] ]
  | _, _ ->
      let with_dmgd =
        if a_c > 0 then
          List.map ~f:(fun perm -> Dmgd :: perm) (permutations (a_c - 1) b_c)
        else []
      in
      let with_op =
        if b_c > 0 then
          List.map ~f:(fun perm -> Oper :: perm) (permutations a_c (b_c - 1))
        else []
      in
      with_dmgd @ with_op

let get_valid_strategies line =
  let free_pos = get_free_pos line |> List.of_array in
  let dmgd_to_place, ops_to_place = free_locs_in_line line in
  let perms = permutations dmgd_to_place ops_to_place in
  List.map perms ~f:(fun perm ->
      let assignments = List.zip_exn free_pos perm in
      let new_locs = Array.copy line.locs in
      List.iter assignments ~f:(fun (pos, loc) -> new_locs.(pos) <- loc);
      let raw =
        Array.map new_locs ~f:loc_to_char |> List.of_array |> String.concat
      in
      { locs = new_locs; stats = line.stats; raw })
  |> List.filter ~f:is_valid_line
  (* |> [%sexp_of: line list] |> Sexp.to_string_hum |> print_endline *)

let () =
  let data = In_channel.read_lines "data" |> List.map ~f:parse_line in

  "????.######..#####. 1,6,5" |> parse_line |> get_valid_strategies |> List.length |> printf "%d valid strategies\n";

  data
  |> List.map ~f:(fun line -> line |> get_valid_strategies |> List.length)
  |> List.fold ~init:0 ~f:( + )
  |> printf "Valid Arrangement Sum: %d\n"
