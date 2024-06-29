open Base
open Stdio

let data = Common.load_data "d13"
let final_answer answer = print_endline ("Final answer: " ^ answer)

let cmp_array left right =
  Array.length left = Array.length right
  && Array.for_all2_exn left right ~f:Char.equal

(** find_mirror finds the mirror idx in the matrix, throws if there is no mirror **)
let find_mirror (matrix : char array array) =
  let rec traverse (started_reflecting : bool) (stack : char array list)
      (rest : char array list) =
    match rest with
    | [] -> List.length stack + ((Array.length matrix - List.length stack) / 2)
    | rest_hd :: rest_tl -> (
        match stack with
        | [] -> traverse false (rest_hd :: stack) rest_tl
        | stack_hd :: stack_tl ->
            (* [%sexp_of: char array] stack_hd |> Sexp.to_string |> print_string; *)
            (* print_string " <stack  rest> "; *)
            (* [%sexp_of: char array] rest_hd |> Sexp.to_string |> print_endline; *)
            if cmp_array stack_hd rest_hd then
              (* print_endline ("found it!" ^ Int.to_string (List.length stack)); *)
              traverse true stack_tl rest_tl
            else if started_reflecting then 0
            else traverse false (rest_hd :: stack) rest_tl)
  in

  traverse false [] (List.of_array matrix)

let file_to_matrices (lines : string list) : char array array list =
  let to_matrix (lines : string list) : char array array =
    lines |> List.rev
    |> List.map ~f:(fun s -> s |> String.to_array)
    |> Array.of_list
  in

  let rec collect (acc : char array array list) (current : string list)
      (rest : string list) =
    match rest with
    | [] -> acc @ [ to_matrix current ]
    | hd :: tl ->
        if String.is_empty hd then collect (acc @ [ to_matrix current ]) [] tl
        else collect acc (hd :: current) tl
  in
  collect [] [] lines

let part1 =
  let find_score (matrix : char array array) =
    let mirror_vert = find_mirror matrix in
    let transpose = Array.transpose_exn matrix in
    let mirror_hori = find_mirror transpose in
    let score = mirror_hori + (mirror_vert * 100) in
    score
  in

  data |> file_to_matrices |> List.map ~f:find_score
  |> List.map ~f:(fun x ->
         print_endline (Int.to_string x);
         x)
  |> List.fold ~init:0 ~f:( + ) |> Int.to_string |> final_answer
