type t = PokerCard.t list
(**RI: A combo is a list of only 5 cards, must be sorted*)

type combo_type =
  | HighCard
  | Pair
  | TwoPair
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  | RoyalFlush

let combo_value t =
  match t with
  | HighCard -> 1
  | Pair -> 2
  | TwoPair -> 3
  | ThreeOfAKind -> 4
  | Straight -> 5
  | Flush -> 6
  | FullHouse -> 7
  | FourOfAKind -> 8
  | StraightFlush -> 9
  | RoyalFlush -> 10

let new_combo (c : PokerCard.t list) : t =
  if List.length c = 5 then List.sort PokerCard.compare c
  else failwith "Invalid combo"

let to_list (c : t) : PokerCard.t list = c

let to_string (c : t) : string =
  let rec to_string' (c : PokerCard.t list) : string =
    match c with
    | [] -> ""
    | h :: t -> PokerCard.to_string h ^ " " ^ to_string' t
  in
  to_string' c

let is_flush combo =
  List.for_all
    (fun x -> PokerCard.suit x = PokerCard.suit (List.hd combo))
    (List.tl combo)

let is_straight combo =
  fst
    (List.fold_left
       (fun (acc, prev) x ->
         ( acc && PokerCard.rank_int_of_card x = prev + 1,
           PokerCard.rank_int_of_card x ))
       (true, PokerCard.rank_int_of_card (List.hd combo))
       (List.tl combo))

let is_four_of_a_kind combo =
  List.length (List.sort_uniq PokerCard.compare_rank combo) = 2
  && PokerCard.rank (List.nth combo 1) = PokerCard.rank (List.nth combo 3)

let is_full_house combo =
  List.length (List.sort_uniq PokerCard.compare_rank combo) = 2
  && PokerCard.rank (List.nth combo 1) <> PokerCard.rank (List.nth combo 3)

let is_three_of_a_kind combo =
  List.length (List.sort_uniq PokerCard.compare_rank combo) = 3
  &&
  let rank_mid = PokerCard.rank (List.nth combo 2) in
  List.length (List.filter (fun x -> PokerCard.rank x = rank_mid) combo) = 3

let is_two_pair combo =
  List.length (List.sort_uniq PokerCard.compare_rank combo) = 3
  &&
  let rank_mid = PokerCard.rank (List.nth combo 2) in
  List.length (List.filter (fun x -> PokerCard.rank x = rank_mid) combo) < 3

let is_one_pair combo =
  List.length (List.sort_uniq PokerCard.compare_rank combo) = 4

let is_straight_flush combo = is_straight combo && is_flush combo

let is_royal_flush combo =
  is_straight_flush combo && PokerCard.rank_int_of_card (List.hd combo) = 10

let check_combo combo =
  if is_royal_flush combo then RoyalFlush
  else if is_straight_flush combo then StraightFlush
  else if is_four_of_a_kind combo then FourOfAKind
  else if is_full_house combo then FullHouse
  else if is_flush combo then Flush
  else if is_straight combo then Straight
  else if is_three_of_a_kind combo then ThreeOfAKind
  else if is_two_pair combo then TwoPair
  else if is_one_pair combo then Pair
  else HighCard

let bro =
  let combo =
    [
      (PokerCard.Hearts, PokerCard.Two);
      (PokerCard.Diamonds, PokerCard.Three);
      (PokerCard.Clubs, PokerCard.Three);
      (PokerCard.Spades, PokerCard.Three);
      (PokerCard.Hearts, PokerCard.Three);
    ]
  in
  if combo_value (check_combo combo) = 8 then 1 else 0
