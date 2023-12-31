open Base
open Stdio

type loc = Oper | Dmgd | Unkn [@@deriving sexp_of]
type line = { locs : loc array; stats : int array } [@@deriving sexp_of]

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
    if Array.length line.stats = 0 then
        true
    else
        false  (* TODO *)

let char_to_loc = function
  | '.' -> Oper
  | '#' -> Dmgd
  | '?' -> Unkn
  | _ -> failwith "Invalid char"

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
      { locs; stats }
  | _ -> failwith "Invalid line"

let rec permutations a_c b_c =
  match (a_c, b_c) with
  | 0, 0 -> [ [] ]
  | _, _ ->
      printf "a_c: %d ; b_c: %d\n" a_c b_c;
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

let get_strategies line =
  let free_pos = get_free_pos line |> List.of_array in
  let dmgd_to_place, ops_to_place = free_locs_in_line line in
  let perms = permutations dmgd_to_place ops_to_place in
  List.map perms ~f:(fun perm ->
      let assignments = List.zip_exn free_pos perm in
      let new_locs = Array.copy line.locs in
      List.iter assignments ~f:(fun (pos, loc) -> new_locs.(pos) <- loc);
      { locs = new_locs; stats = line.stats })
  |> List.filter ~f:is_valid_line
  |> [%sexp_of: line list] |> Sexp.to_string_hum |> print_endline

let () =
  let data = In_channel.read_lines "data" |> List.map ~f:parse_line in

  "???..###. 1,1,3" |> parse_line |> get_strategies;
  ignore is_valid_line;

  data |> List.map ~f:free_locs_in_line |> ignore;

  (* |> List.iter ~f:(fun (a, b) -> printf "dmg: %d ; ops: %d\n" a b); *)
  data |> List.map ~f:get_free_pos
  |> List.iter ~f:(fun x ->
         x |> Array.map ~f:Int.to_string |> Array.to_list
         |> String.concat ~sep:"," |> ignore)
(* |> print_endline) *)

(* data |> [%sexp_of: line list] |> Sexp.to_string_hum |> print_endline *)
