open Base
open Stdio

let find_all_indices line word =
  let rec find_all_indices' line word acc curr_index =
    match String.substr_index line ~pattern:word with
    | None -> acc
    | Some index ->
        let new_index = index + curr_index in
        find_all_indices'
          (String.drop_prefix line (new_index + 1))
          word (new_index :: acc)
          (curr_index + new_index + 1)
  in
  find_all_indices' line word [] 0 |> List.rev

let word_and_digit =
  [
    ("zero", 0);
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
    ("1", 1);
    ("2", 2);
    ("3", 3);
    ("4", 4);
    ("5", 5);
    ("6", 6);
    ("7", 7);
    ("8", 8);
    ("9", 9);
    ("0", 0);
  ]

let find_all_numbers_and_their_indices line =
  let res =
    List.map word_and_digit ~f:(fun (word, digit) ->
        (digit, find_all_indices line word))
  in
  let nonempty =
    List.filter res ~f:(fun (_, indices) -> not (List.is_empty indices))
  in
  let merged =
    List.map nonempty ~f:(fun (digit, indices) ->
        List.map indices ~f:(fun index -> (digit, index)))
  in
  List.concat merged
  |> List.sort ~compare:(fun (_, i1) (_, i2) -> Int.compare i1 i2)

let first_and_last lst =
  match lst with
  | [] -> None
  | hd :: [] -> Some (hd, hd)
  | hd :: tl -> Some (hd, List.last_exn tl)

let parse_line line =
  find_all_numbers_and_their_indices line
  |> first_and_last
  |> (fun x ->
       match x with
       | None -> failwith "No numbers found"
       | Some ((fst_digit, _), (lst_digit, _)) -> (fst_digit, lst_digit))
  |> (fun (fst_digit, lst_digit) -> [ fst_digit; lst_digit ])
  |> List.map ~f:(fun digit -> Int.to_string digit)
  |> String.concat ~sep:"" |> Int.of_string

let read_all_lines channel =
  let rec read_all_lines' channel acc =
    match In_channel.input_line channel with
    | None -> acc
    | Some line -> read_all_lines' channel (line :: acc)
  in
  read_all_lines' channel [] |> List.rev

(* let lines = [ "123onetwothreeseven"; "onetwothreefourfivesixseven" ] *)

let () =
  In_channel.create "data" |> read_all_lines |> List.map ~f:parse_line
  |> List.sum (module Int) ~f:Fn.id
  |> Stdio.printf "%d\n"
