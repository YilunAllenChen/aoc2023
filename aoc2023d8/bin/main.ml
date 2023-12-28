open Base
open Stdio

type node = { curr : string; left : string; right : string } [@@deriving sexp]
type dir = Left | Right [@@deriving sexp]

let char_to_dir = function
  | 'L' -> Left
  | 'R' -> Right
  | _ -> failwith "Invalid direction"

let find_next lookup curr dir =
  match Map.find lookup curr with
  | Some node -> ( match dir with Left -> node.left | Right -> node.right)
  | None -> failwith "Invalid node"

let parse_line line =
  match String.split line ~on:'=' with
  | [ curr; left_and_right ] -> (
      let left_and_right =
        left_and_right
        |> String.strip ~drop:(Char.equal ' ')
        |> String.strip ~drop:(Char.equal '(')
        |> String.strip ~drop:(Char.equal ')')
        |> String.split ~on:','
      in
      match left_and_right with
      | [ left; right ] ->
          {
            curr = curr |> String.strip;
            left = left |> String.strip;
            right = right |> String.strip;
          }
      | _ -> failwith "Invalid line")
  | _ -> failwith "Invalid line"

let end_with_a node = String.to_list node |> List.last_exn |> Char.equal 'A'
let end_with_z node = String.to_list node |> List.last_exn |> Char.equal 'Z'

let () =
  let lines = In_channel.read_lines "data" in
  let steps, nodes =
    match lines with
    | steps_chars :: _ :: rest ->
        let steps =
          steps_chars |> String.strip |> String.to_list
          |> List.map ~f:char_to_dir
        in

        let nodes = rest |> List.map ~f:parse_line in
        (steps, nodes)
    | _ -> failwith "Invalid data"
  in
  steps
  |> List.iter ~f:(fun step ->
         printf "%s\n" (Sexp.to_string_hum (sexp_of_dir step)));

  let lookup =
    List.fold nodes
      ~init:(Map.empty (module String))
      ~f:(fun acc node -> Map.add_exn acc ~key:node.curr ~data:node)
  in

  let rec walk acc steps_left ~curr ~check =
    match steps_left with
    | [] -> walk acc steps ~curr ~check
    | step :: rest ->
        let next = find_next lookup curr step in
        if check next then acc + 1 else walk (acc + 1) rest ~curr:next ~check
  in
  let match_target = String.equal "ZZZ" in
  walk 0 steps ~curr:"AAA" ~check:match_target |> printf "pt1: %d\n";

  (* pt2 *)
  (* these individual starts correspond to unique ends *)
  let rec gcd a b = if b = 0 then abs a else gcd b (a % b) in
  let lcm a b = abs a * abs b / gcd a b in

  let starting_nodes =
    List.filter nodes ~f:(fun node -> end_with_a node.curr)
    |> List.map ~f:(fun node -> node.curr)
  in
  starting_nodes
  |> List.map ~f:(fun n -> walk 0 steps ~curr:n ~check:end_with_z)
  |> List.fold ~init:1 ~f:lcm |> printf "pt: %d\n"
