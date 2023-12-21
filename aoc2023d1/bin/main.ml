let rec last seq =
  match seq () with
  | Seq.Nil -> None
  | Seq.Cons (x, rest) -> (
      match rest () with Seq.Nil -> Some x | _ -> last rest)

let fst seq = match seq () with Seq.Nil -> None | Seq.Cons (x, _) -> Some x

let rec process_chan channel cumsum =
  match input_line channel with
  | line ->
      let this_value =
        line |> String.to_seq
        |> Seq.filter (fun ch ->
               let code = Char.code ch in
               code >= 48 && code <= 57)
        |> (fun seq ->
             match (fst seq, last seq) with
             | Some first, Some last -> [ first; last ]
             | _ -> [])
        |> List.to_seq |> String.of_seq |> int_of_string
      in
      process_chan channel (cumsum + this_value)
  | exception End_of_file ->
      close_in channel;
      cumsum

let read_file filename =
  let channel = open_in filename in
  let cumsum = process_chan channel 0 in
  print_int cumsum;
  print_newline ()

let () = read_file "data"
