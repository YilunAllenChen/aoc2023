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


type neighbors = { left_hand : (int * int) list; right_hand : (int * int) list }

let get_loc tmap (x, y) = Map.find_exn tmap (x, y)

let in_map tmap coord = match Map.find tmap coord with Some _ -> true | None -> false

let neighbor_tiles_to_check c come_from tmap =
  let just_right = [ (c.x + 1, c.y) ] in
  let just_left = [ (c.x - 1, c.y) ] in
  let just_up = [ (c.x, c.y - 1) ] in
  let just_down = [ (c.x, c.y + 1) ] in
  let just_tr = [ (c.x + 1, c.y - 1) ] in
  let just_tl = [ (c.x - 1, c.y - 1) ] in
  let just_bl = [ (c.x - 1, c.y + 1) ] in
  let just_br = [ (c.x + 1, c.y + 1) ] in
  let all_trs = [ (c.x + 1, c.y); (c.x, c.y - 1); (c.x + 1, c.y - 1) ] in
  let all_bls = [ (c.x - 1, c.y); (c.x, c.y + 1); (c.x - 1, c.y + 1) ] in
  let all_brs = [ (c.x + 1, c.y); (c.x, c.y + 1); (c.x + 1, c.y + 1) ] in
  let all_tls = [ (c.x - 1, c.y); (c.x, c.y - 1); (c.x - 1, c.y - 1) ] in
  let in_tmap = in_map tmap in
  let raw_locs =
    match (c.tile, come_from) with
    | UpDown, Up -> { left_hand = just_right; right_hand = just_left }
    | UpDown, Down -> { left_hand = just_left; right_hand = just_right }
    | LeftRight, Left -> { left_hand = just_up; right_hand = just_down }
    | LeftRight, Right -> { left_hand = just_down; right_hand = just_up }
    | TopRight, Up -> { left_hand = just_tr; right_hand = all_bls }
    | TopRight, Right -> { left_hand = all_bls; right_hand = just_tr }
    | TopLeft, Up -> { left_hand = all_brs; right_hand = just_tl }
    | TopLeft, Left -> { left_hand = just_tl; right_hand = all_brs }
    | BottomLeft, Left -> { left_hand = all_trs; right_hand = just_bl }
    | BottomLeft, Down -> { left_hand = just_bl; right_hand = all_trs }
    | BottomRight, Right -> { left_hand = just_br; right_hand = all_tls }
    | BottomRight, Down -> { left_hand = all_tls; right_hand = just_br }
    | Start, Down -> { left_hand = just_left; right_hand = just_right }
    | Start, Up -> { left_hand = just_right; right_hand = just_left }
    | Start, Left -> { left_hand = just_up; right_hand = just_down }
    | Start, Right -> { left_hand = just_down; right_hand = just_up }
    | _ -> failwith "Unhandled"
  in
  let raw_lefts =
    List.fold ~init:[] ~f:(fun acc ele -> ele :: acc) raw_locs.left_hand
  in
  let lefts = List.filter ~f:in_tmap raw_lefts in
  let raw_rights =
    List.fold ~init:[] ~f:(fun acc ele -> ele :: acc) raw_locs.right_hand
  in
  let rights = List.filter ~f:in_tmap raw_rights in
  (lefts, rights)

(* let build_map lines = *)
(*   let rec build_map' lines y acc = *)
(*     match lines with *)
(*     | [] -> acc *)
(*     | line :: lines -> *)
(*         let rec build_map'' line x acc = *)
(*           match line with *)
(*           | [] -> acc *)
(*           | char :: line -> *)
(*               let tile = char_to_tile char in *)
(*               let acc = { x; y; tile } :: acc in *)
(*               build_map'' line (x + 1) acc *)
(*         in *)
(*         let acc = build_map'' line 0 acc in *)
(*         build_map' lines (y + 1) acc *)
(*   in *)
(*   let all_locs = build_map' lines 0 [] in *)
(*   let map = Map.empty (module IntPairComparator) in *)
(*   List.fold all_locs ~init:map ~f:(fun map { x; y; tile } -> *)
(*       Map.set map ~key:(x, y) ~data:{ x; y; tile }) *)

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
              build_map'' line (x + 1) ({ x; y; tile } :: acc)
        in
        build_map' lines (y + 1) (build_map'' line 0 acc)
  in
  let all_locs = build_map' lines 0 [] in
  let map = Map.empty (module IntPairComparator) in
  List.fold_left ~init:map ~f:(fun map { x; y; tile } ->
      Map.set map ~key:(x, y) ~data:{ x; y; tile }) all_locs

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
      | _ -> failwith ("Invalid tile from up. now at " ^ (curr |> sexp_of_loc |>Sexp.to_string_hum )))
  | Down -> (
      match curr.tile with
      | UpDown -> (get_loc tmap (curr.x, curr.y - 1), Down)
      | BottomLeft -> (get_loc tmap (curr.x - 1, curr.y), Right)
      | BottomRight -> (get_loc tmap (curr.x + 1, curr.y), Left)
      | _ -> failwith "Invalid tile from down")

let traverse tmap ~init ~starting_from =
  let rec keep_going paths curr tmap come_from =
    (* printf "x: %d, y: %d, tile: %s. Came from: %s; " curr.x curr.y *)
    (*   (Sexp.to_string_hum (sexp_of_tile curr.tile)) *)
    (*   (Sexp.to_string_hum (sexp_of_came_from come_from)); *)
    (**)
    (* neighbor_tiles_to_check curr come_from tmap *)
    (* |> [%sexp_of: (int * int) list * (int * int) list] |> Sexp.to_string_hum *)
    (* |> printf "lefts and rights: %s\n"; *)
    (**)
    if phys_equal curr.tile Start then List.append paths [ (come_from, curr) ]
    else
      let next, next_from = find_next_and_next_from tmap curr come_from in
      let acc_path = List.append paths [ (come_from, curr) ] in
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
  | { tile = TopRight; _ } -> (next, Right)
  | { tile = BottomRight; _ } -> (next, Right)
  | _ -> (
      let next = get_loc tmap (x + 1, y) in
      match next with
      | { tile = LeftRight; _ } -> (next, Left)
      | { tile = TopLeft; _ } -> (next, Left)
      | { tile = BottomLeft; _ } -> (next, Left)
      | _ -> (
          let next = get_loc tmap (x, y + 1) in
          match get_loc tmap (x, y - 1) with
          | { tile = UpDown; _ } -> (next, Down)
          | { tile = BottomLeft; _ } -> (next, Down)
          | { tile = BottomRight; _ } -> (next, Down)
          | _ -> (
              let next = get_loc tmap (x, y - 1) in
              match next with
              | { tile = UpDown; _ } -> (next, Up)
              | { tile = TopLeft; _ } -> (next, Up)
              | { tile = TopRight; _ } -> (next, Up)
              | _ -> failwith "Invalid start")))

let print_ill iil = iil |> [%sexp_of: (int * int) list] |> Sexp.to_string_hum |> print_endline

let flood_from start tmap wall =
  let (x, y) = start in
  printf "flodding %d %d\n" x y;
  let is_wall (x, y) =
    match Map.find wall (x, y) with Some _ -> true | None -> false
  in
  let seen = Map.empty (module IntPairComparator) in
  let should_include coords = (in_map tmap coords && not @@ is_wall coords) in
  let rec flood seen flooded to_travel =
    match to_travel with
    | [] -> flooded
    | (x, y) :: rest ->
      let new_to_travel, new_seen = match Map.find seen (x, y) with
        | None -> (
          let maybe_right = (x + 1, y) in
          let maybe_down = (x, y + 1) in
          (rest
            @ (if should_include maybe_down then [ maybe_down ] else [])
            @ if should_include maybe_right then [ maybe_right ] else []),
          Map.set seen ~key:(x, y) ~data:true
        )
        | Some _ -> rest, seen
          in
          flood new_seen ((x, y) :: flooded) new_to_travel
  in

  flood seen [] [ start ]

let dedup_coords =
  List.dedup_and_sort ~compare:(fun (x1, y1) (x2, y2) ->
      match compare x1 x2 with 0 -> compare y1 y2 | v -> v)


let () =
  print_endline "";
  let data = In_channel.read_lines "data" in

  let tmap = data |> List.map ~f:String.to_list |> build_map in
  printf "done building map\n";

  let start, starting_from = find_a_start tmap in

  (* pt 1 *)
  let path = traverse tmap ~init:start ~starting_from in

  path
  |> List.map ~f:(fun (_, { x; y; _ }) -> (x, y))
  |> (fun path -> List.length path / 2)
  |> printf "pt1: %d\n";

  let wall = List.map path ~f:(fun (_, step) -> (step.x, step.y)) |> List.fold ~init:(Map.empty (module IntPairComparator)) ~f:(fun acc (x, y) -> Map.set acc ~key:(x, y) ~data:true ) in

  let wall_coords = Map.keys wall in
  let drop_wall lst =
    List.filter ~f:(
      fun (x1, y1) -> not (List.exists wall_coords ~f:(fun (x2, y2) -> (x1 = x2 && y1 = y2)))
    ) lst
  in

  (* pt 2 *)
  let lefts, rights =
    traverse tmap ~init:start ~starting_from
    |> List.map ~f:(fun (from, loc) -> neighbor_tiles_to_check loc from tmap)
    |> List.fold ~init:([], []) ~f:(fun (acc_l, acc_r) (ele_l, ele_r) ->
           (acc_l @ ele_l, acc_r @ ele_r))
  in
  let flt_lefts = lefts |> dedup_coords |> drop_wall in
  let flt_rights = rights |> dedup_coords |> drop_wall in
  print_endline "done plotting lefts and rights";
  print_endline "done plotting wall";


  let outside = flood_from (0, 0) tmap wall in
  print_endline "done plotting outside";
  outside |> print_ill;
  
  print_endline "lefts";
  flt_lefts |> print_ill;

  print_endline "rights";
  flt_rights |> print_ill;

  let inside =
    if
      List.exists flt_rights ~f:(fun (rx, ry) ->
          List.exists outside ~f:(fun (fx, fy) -> rx = fx && ry = fy))
    then flt_lefts
    else flt_rights
  in
  printf "inside is %s\n" (if phys_equal inside flt_lefts then "left" else "right");

  printf "now flooding inside";
  let flooded_inside =
    List.map inside ~f:(fun node -> flood_from node tmap wall)
  in

  flooded_inside |> List.concat |> dedup_coords |> drop_wall
  |> List.length
  |> printf "total number of flooded: %d\n"
