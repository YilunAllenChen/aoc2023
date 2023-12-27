open Base
open Stdio

type card = Ace | King | Queen | Jack | Ten | Number of int [@@deriving sexp]
type hand = { cards : card list; bid : int } [@@deriving sexp]

let card_to_int = function
  | Ace -> 14
  | King -> 13
  | Queen -> 12
  | Jack -> 11
  | Ten -> 10
  | Number n -> n

let char_to_card = function
  | 'A' -> Ace
  | 'K' -> King
  | 'Q' -> Queen
  | 'J' -> Jack
  | 'T' -> Ten
  | c -> Number (Int.of_string @@ String.of_char c)

let card_to_int_2 = function
  | Ace -> 14
  | King -> 13
  | Queen -> 12
  | Jack -> 0 (* joker *)
  | Ten -> 10
  | Number n -> n

let parse_hand line =
  match
    String.split line ~on:' '
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  with
  | [ cards_str; bid_str ] ->
      let cards = List.map (String.to_list cards_str) ~f:char_to_card in
      let bid = Int.of_string bid_str in
      { cards; bid }
  | _ -> failwith "Invalid input"

let group_cards cards =
  let cards =
    List.sort cards ~compare:(fun a b -> card_to_int b - card_to_int a)
  in
  let rec helper cards_left all_groups curr_group =
    match cards_left with
    | [] -> curr_group :: all_groups
    | card :: rest -> (
        match curr_group with
        | [] -> helper rest all_groups [ card ]
        | curr_card :: _ ->
            if card_to_int card = card_to_int curr_card then
              helper rest all_groups (card :: curr_group)
            else helper rest (curr_group :: all_groups) [ card ])
  in
  helper cards [] []

let score card_groups =
  let lens = List.map card_groups ~f:List.length in
  let sorted_lens = List.sort lens ~compare:(fun a b -> compare b a) in
  match sorted_lens with
  | [ 5 ] -> 7
  | [ 4; 1 ] -> 6
  | [ 3; 2 ] -> 5
  | [ 3; 1; 1 ] -> 4
  | [ 2; 2; 1 ] -> 3
  | 2 :: _ -> 2
  | _ -> 1

let score_with_joker card_groups =
  let is_jack = function Jack -> true | _ -> false in
  let sorted_lens =
    card_groups
    |> List.filter ~f:(fun group -> not (List.exists group ~f:is_jack))
    |> List.map ~f:List.length
    |> List.sort ~compare:(fun a b -> compare b a)
  in
  let joker_count =
    match List.exists card_groups ~f:(List.exists ~f:is_jack) with
    | true ->
        List.find_exn card_groups ~f:(List.exists ~f:is_jack) |> List.length
    | false -> 0
  in
  match joker_count with
  | 0 -> score card_groups
  | 5 -> 7
  | 4 -> 7
  | 3 -> (
      match sorted_lens with
      | [ 2 ] -> 7 (* five of a kind *)
      | [ 1; 1 ] -> 6 (* four of a kind *)
      | _ -> failwith "corrupted 2-card data")
  | 2 -> (
      match sorted_lens with
      | [ 3 ] -> 7 (* five of a kind *)
      | [ 2; 1 ] -> 6 (* four of a kind *)
      | [ 1; 1; 1 ] -> 4 (* three of a kind *)
      | _ -> failwith "corrupted 3-card data")
  | 1 -> (
      match sorted_lens with
      | [ 4 ] -> 7 (* five of a kind *)
      | [ 3; 1 ] -> 6 (* four of a kind *)
      | [ 2; 2 ] -> 5 (* full house *)
      | [ 2; 1; 1 ] -> 4 (* three of a kind *)
      | [ 1; 1; 1; 1 ] -> 2 (* two of a kind *)
      | _ -> failwith "corrupted 4-card data")
  | _ -> failwith "corrupted data"

let rec compare_indivual_cards_one_by_one cards1 cards2 card_to_int_fun =
  match (cards1, cards2) with
  | card1 :: rest1, card2 :: rest2 -> (
      match Int.compare (card_to_int_fun card1) (card_to_int_fun card2) with
      | 0 -> compare_indivual_cards_one_by_one rest1 rest2 card_to_int_fun
      | n -> n)
  | _ -> failwith "Invalid cards"

let sort_hands hand1 hand2 ~ctoi ~scoring_fun =
  let score_hand hand = group_cards hand.cards |> scoring_fun in
  match Int.compare (hand1 |> score_hand) (hand2 |> score_hand) with
  | 0 -> compare_indivual_cards_one_by_one hand1.cards hand2.cards ctoi
  | n -> n

let () =
  let data = In_channel.read_lines "data" in
  let hands = List.map ~f:parse_hand data in
  let earning hand rank = hand.bid * rank in

  (* part 2 *)
  hands
  |> List.sort ~compare:(fun hand1 hand2 ->
         sort_hands ~scoring_fun:score ~ctoi:card_to_int hand1 hand2)
  |> List.mapi ~f:(fun i hand -> earning hand (i + 1))
  |> List.fold ~init:0 ~f:( + ) |> printf "pt1: %d\n";

  (* part 2 *)
  hands
  |> List.sort ~compare:(fun hand1 hand2 ->
         sort_hands ~scoring_fun:score_with_joker ~ctoi:card_to_int_2 hand1 hand2)
  |> List.mapi ~f:(fun i hand -> earning hand (i + 1))
  |> List.fold ~init:0 ~f:( + ) |> printf "pt2: %d\n"
