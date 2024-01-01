open Base
open Stdio

type island = { orig : string list; transposed : string list }
[@@deriving sexp_of]

let to_blocks lines =
  let rec conitnue_reading acc curr rest =
    match rest with
    | [] -> curr :: List.rev acc
    | this :: rest -> (
        match String.is_empty this with
        | true -> conitnue_reading (List.rev curr :: acc) [] rest
        | false -> conitnue_reading acc (this :: curr) rest)
  in
  conitnue_reading [] [] lines

let transpose matrix =
  if Array.length matrix = 0 || Array.length matrix.(0) = 0 then [||]
  else
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in
    (* matrix |> [%sexp_of: char array array] |> Sexp.to_string_hum *)
    (* |> print_endline; *)
    Array.init cols ~f:(fun i -> Array.init rows ~f:(fun j -> matrix.(j).(i)))

let block_to_island block =
  let orig = block in
  let mtx =
    List.map block ~f:(fun line -> line |> String.to_list |> List.to_array)
    |> Array.of_list
  in
  let transposed =
    transpose mtx |> Array.map ~f:String.of_array |> Array.to_list |> List.rev
  in
  { orig; transposed }

let find_mirror_in str_lst =
  let rec find stack rest =
    match (stack, rest) with
    | [], hd :: tl -> find [ hd ] tl
    | _, [] -> None
    | stack, rest ->
        stack |> [%sexp_of: string list] |> Sexp.to_string_hum |> printf "%s\n";
        rest |> [%sexp_of: string list] |> Sexp.to_string_hum |> printf "%s\n\n";
        let is_palindrome =
          List.is_prefix rest ~prefix:stack ~equal:(fun a b -> String.equal a b)
          || List.is_prefix stack ~prefix:rest ~equal:(fun a b ->
                 String.equal a b)
        in
        if is_palindrome then Some (List.length rest)
        else find (List.hd_exn rest :: stack) (List.tl_exn rest)
  in
  find [] str_lst

let find_mirror_in island =
  match find_mirror_in island.orig with
  | Some num ->
      printf "found horizontal mirror at %d\n" num;
      num * 100
  | None -> (
      match find_mirror_in island.transposed with
      | Some num ->
          printf "found vertical mirror at %d\n" num;
          num
      | None -> failwith "no mirror in this area...")

let () =
  let blocks = In_channel.read_lines "data" |> to_blocks in

  (* blocks |> [%sexp_of: string list list] |> Sexp.to_string_hum |> print_endline; *)
  blocks
  |> List.map ~f:(fun line -> line |> block_to_island |> find_mirror_in)
  |> List.fold ~init:0 ~f:( + ) |> printf "pt1: %d\n"
