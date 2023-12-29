open Base
open Stdio

type tile =
  | LeftRight
  | UpDown
  | TopLeft
  | TopRight
  | BottomLeft
  | BottomRight
  | Ground
  | Start
[@@deriving sexp_of]

type came_from = Left | Right | Up | Down [@@deriving sexp_of]

let char_to_tile = function
  | '-' -> LeftRight
  | '|' -> UpDown
  | '.' -> Ground
  | 'J' -> TopLeft
  | 'L' -> TopRight
  | '7' -> BottomLeft
  | 'F' -> BottomRight
  | 'S' -> Start
  | _ -> failwith "Invalid tile"

type loc = { x : int; y : int; tile : tile } [@@deriving sexp_of]

module IntPairComparator = struct
  module T = struct
    type t = int * int [@@deriving sexp_of]

    let compare (x1, y1) (x2, y2) =
      match Int.compare x1 x2 with 0 -> Int.compare y1 y2 | x -> x
  end

  include T
  include Comparator.Make (T)
end

type loc_map = loc Map.M(IntPairComparator).t [@@deriving sexp_of]

let build_map lines =
  let rec build_map' lines y acc =
    match lines with
    | [] -> acc
    | line :: lines ->
        let rec build_map'' line x acc =
          match line with
          | [] -> acc
          | char :: line ->
              let tile = char_to_tile char in
              let acc = { x; y; tile } :: acc in
              build_map'' line (x + 1) acc
        in
        let acc = build_map'' line 0 acc in
        build_map' lines (y + 1) acc
  in
  let all_locs = build_map' lines 0 [] in
  let map = Map.empty (module IntPairComparator) in
  List.fold all_locs ~init:map ~f:(fun map { x; y; tile } ->
      Map.set map ~key:(x, y) ~data:{ x; y; tile })

let get_loc tmap (x, y) = Map.find_exn tmap (x, y)

let find_next_and_next_from tmap curr come_from =
  match come_from with
  | Left -> (
      match curr.tile with
      | LeftRight -> (get_loc tmap (curr.x + 1, curr.y), Left)
      | TopLeft -> (get_loc tmap (curr.x, curr.y - 1), Down)
      | BottomLeft -> (get_loc tmap (curr.x, curr.y + 1), Up)
      | _ -> failwith "Invalid tile from left")
  | Right -> (
      match curr.tile with
      | LeftRight -> (get_loc tmap (curr.x - 1, curr.y), Right)
      | TopRight -> (get_loc tmap (curr.x, curr.y - 1), Down)
      | BottomRight -> (get_loc tmap (curr.x, curr.y + 1), Up)
      | _ -> failwith "Invalid tile from right")
  | Up -> (
      match curr.tile with
      | UpDown -> (get_loc tmap (curr.x, curr.y + 1), Up)
      | TopLeft -> (get_loc tmap (curr.x - 1, curr.y), Right)
      | TopRight -> (get_loc tmap (curr.x + 1, curr.y), Left)
      | _ -> failwith "Invalid tile from up")
  | Down -> (
      match curr.tile with
      | UpDown -> (get_loc tmap (curr.x, curr.y - 1), Down)
      | BottomLeft -> (get_loc tmap (curr.x - 1, curr.y), Right)
      | BottomRight -> (get_loc tmap (curr.x + 1, curr.y), Left)
      | _ -> failwith "Invalid tile from down")

let traverse tmap ~init ~starting_from =
  let rec keep_going paths curr tmap come_from =
    printf "x: %d, y: %d, tile: %s. Came from: %s\n" curr.x curr.y
      (Sexp.to_string_hum (sexp_of_tile curr.tile))
      (Sexp.to_string_hum (sexp_of_came_from come_from));
    if phys_equal curr.tile Start then List.append paths [ curr ]
    else
      let next, next_from = find_next_and_next_from tmap curr come_from in
      let acc_path = List.append paths [ curr ] in
      keep_going acc_path next tmap next_from
  in
  keep_going [] init tmap starting_from

let find_a_start tmap =
  let start =
    Map.filter tmap ~f:(fun { tile; _ } -> phys_equal tile Start)
    |> Map.data |> List.hd_exn
  in
  printf "start: %s\n" (Sexp.to_string_hum (sexp_of_loc start));
  let { x; y; _ } = start in
  let next = get_loc tmap (x - 1, y) in
  match next with
  | { tile = LeftRight; _ } -> (next, Right)
  | { tile = TopRight; _ } -> (next, Down)
  | { tile = BottomRight; _ } -> (next, Up)
  | _ -> (
      let next = get_loc tmap (x + 1, y) in
      match next with
      | { tile = LeftRight; _ } -> (next, Left)
      | { tile = TopLeft; _ } -> (next, Down)
      | { tile = BottomLeft; _ } -> (next, Up)
      | _ -> (
          let next = get_loc tmap (x, y - 1) in
          match get_loc tmap (x, y - 1) with
          | { tile = UpDown; _ } -> (next, Down)
          | { tile = BottomLeft; _ } -> (next, Right)
          | { tile = BottomRight; _ } -> (next, Left)
          | _ -> (
              let next = get_loc tmap (x, y + 1) in
              match next with
              | { tile = UpDown; _ } -> (next, Up)
              | { tile = TopLeft; _ } -> (next, Right)
              | { tile = TopRight; _ } -> (next, Left)
              | _ -> failwith "Invalid start")))

let () =
  let data = In_channel.read_lines "data" in

  let tmap = data |> List.map ~f:String.to_list |> build_map in
  let start, starting_from = find_a_start tmap in
  printf "start: %s\n" (Sexp.to_string_hum (sexp_of_loc start));
  printf "starting_from: %s\n"
    (Sexp.to_string_hum (sexp_of_came_from starting_from));

  (* pt 1 *)
  traverse tmap ~init:start ~starting_from
  |> List.map ~f:(fun { x; y; _ } -> (x, y))
  |> (fun path -> List.length path / 2)
  |> printf "pt1: %d\n"
