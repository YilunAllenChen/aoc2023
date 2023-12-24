(* open Base *)
open Core

let () = In_channel.read_lines "data" |> List.iter ~f:print_endline
