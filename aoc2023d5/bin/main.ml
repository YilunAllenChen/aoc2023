open Base
open Stdio

let make_lookup triples target =
  let rec loop triples_left target =
    match triples_left with
    | [] -> target
    | (to_, from_, range) :: rest ->
        if from_ <= target && target < from_ + range then target - from_ + to_
        else loop rest target
  in
  loop triples target

let make_rev_lookup triples target =
  let rec loop triples_left target =
    match triples_left with
    | [] -> target
    | (to_, from_, range) :: rest ->
        if to_ <= target && target < to_ + range then target + from_ - to_
        else loop rest target
  in
  loop triples target

let parse_head head =
  let parts = String.split head ~on:':' in
  match parts with
  | [ _; seeds_str ] ->
      seeds_str |> String.split ~on:' '
      |> List.filter ~f:(fun s -> String.length s > 0)
      |> List.map ~f:Int.of_string
  | _ -> failwith "bad header"

let parse_section section =
  let section = section |> List.filter ~f:(fun s -> String.length s > 0) in
  match section with
  | map_name :: number_lines ->
      let from_t, to_t =
        match String.split map_name ~on:' ' with
        | [ relationship; _ ] -> (
            match String.split relationship ~on:'-' with
            | [ from_t; _; to_t ] -> (from_t, to_t)
            | _ -> failwith "bad relationship")
        | _ -> failwith "bad section header"
      in

      let to_triple line =
        let splitted =
          line |> String.split ~on:' '
          |> List.filter ~f:(fun s -> String.length s > 0)
        in
        match splitted with
        | [ from_t; to_t; range ] ->
            (Int.of_string from_t, Int.of_string to_t, Int.of_string range)
        | _ -> failwith "bad line"
      in

      let triples = List.map ~f:to_triple number_lines in

      (from_t, to_t, make_lookup triples, make_rev_lookup triples)
  | _ -> failwith "bad section"

let divide_into_sections lines =
  let rec loop all_sections acc lines =
    match lines with
    | [] -> acc :: all_sections
    | hd :: tl -> (
        match String.length hd with
        | 0 -> loop (acc :: all_sections) [] tl
        | _ -> loop all_sections (hd :: acc) tl)
  in
  let raw_sections = loop [] [] lines in
  raw_sections
  |> List.filter ~f:(fun section -> List.length section > 0)
  |> List.rev
  |> List.map ~f:(fun section -> List.rev section)

let lookup_location_for_seed seed lookups =
  let rec loop curr_t curr_v =
    let _, to_t, func, _ =
      List.find_exn lookups ~f:(fun (from_t, _, _, _) ->
          String.equal from_t curr_t)
    in
    match to_t with "location" -> func curr_v | _ -> loop to_t (func curr_v)
  in
  loop "seed" seed

let reverse_lookup_location_for_loc seed lookups =
  let rec loop curr_t curr_v =
    (* printf "curr_t: %s, curr_v: %d\n" curr_t curr_v; *)
    let from_t, _, _, func =
      List.find_exn lookups ~f:(fun (_, to_t, _, _) -> String.equal to_t curr_t)
    in
    let next_v = func curr_v in
    match from_t with "seed" -> next_v | _ -> loop from_t next_v
  in
  loop "location" seed

let merge_ranges tuples =
  let rec loop acc tuples_left =
    match tuples_left with
    | [] -> acc
    | (from1, to1) :: (from2, to2) :: rest ->
        if from2 <= to1 then loop acc ((from1, max to1 to2) :: rest)
        else loop ((from1, to1) :: acc) ((from2, to2) :: rest)
    | [ (from, to_) ] -> (from, to_) :: acc
  in
  tuples
  |> List.sort ~compare:(fun (from1, _) (from2, _) -> Int.compare from1 from2)
  |> loop []

let collect_ranges line =
  let rec loop acc line_left =
    match line_left with
    | [] -> acc
    | from :: range :: rest ->
        if from = range then loop acc rest
        else loop ((from, from + range) :: acc) rest
    | _ -> failwith "bad line"
  in
  line |> loop [] |> merge_ranges

let in_any_range ranges loc =
  List.exists ranges ~f:(fun (from, to_) -> from <= loc && loc <= to_)

let () =
  match In_channel.read_lines "data" with
  | [] -> printf "No data\n"
  | hd :: _ :: rest ->
      let seeds = parse_head hd in
      let lookups = divide_into_sections rest |> List.map ~f:parse_section in

      (* pt 1 *)
      seeds
      |> List.map ~f:(fun seed -> (seed, lookup_location_for_seed seed lookups))
      |> List.sort ~compare:(fun (_, loc1) (_, loc2) -> Int.compare loc1 loc2)
      |> List.hd_exn
      |> fun (seed, loc) ->
      printf "pt1: %d -> %d\n" seed loc;

      (* pt 2 *)
      let loc_scan = ref 45 in

      (* let min_loc = ref 0 in *)
      (* let seed = ref 0 in *)
      let merged = collect_ranges seeds |> merge_ranges in

      let found = ref false in
      while not !found do
        let seed = reverse_lookup_location_for_loc !loc_scan lookups in
        if in_any_range merged seed then (
          printf "pt2: location: %d -> seed: %d\n" !loc_scan seed;
          found := true;
          loc_scan := !loc_scan + 1)
        else loc_scan := !loc_scan + 1
      done
  | _ -> failwith "bad data"
