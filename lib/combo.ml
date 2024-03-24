type t = Card.t list

(** RI: A combo is a list of only 5 cards, must be sorted *)

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

let new_combo (c : Card.t list) : t =
  if List.length c = 5 then List.sort Card.compare c
  else failwith "Invalid combo"

let to_list (c : t) : Card.t list = c

let to_string (c : t) : string =
  let rec to_string' (c : Card.t list) : string =
    match c with
    | [] -> ""
    | h :: t -> Card.to_string h ^ " " ^ to_string' t
  in
  to_string' c

let is_flush combo =
  List.for_all
    (fun x -> Card.suit x = Card.suit (List.hd combo))
    (List.tl combo)

let is_straight combo =
  fst
    (List.fold_left
       (fun (acc, prev) x ->
         (acc && Card.rank_int_of_card x = prev + 1, Card.rank_int_of_card x))
       (true, Card.rank_int_of_card (List.hd combo))
       (List.tl combo))

let is_four_of_a_kind combo =
  List.length (List.sort_uniq Card.compare_rank combo) = 2
  && Card.rank (List.nth combo 1) <> Card.rank (List.nth combo 3)

let is_full_house combo =
  List.length (List.sort_uniq Card.compare_rank combo) = 2
  && Card.rank (List.nth combo 1) = Card.rank (List.nth combo 3)

let is_three_of_a_kind combo =
  List.length (List.sort_uniq Card.compare combo) = 3
  &&
  let rank_mid = Card.rank (List.nth combo 2) in
  List.length (List.filter (fun x -> Card.rank x = rank_mid) combo) = 3

let is_two_pair combo =
  List.length (List.sort_uniq Card.compare combo) = 3
  &&
  let rank_mid = Card.rank (List.nth combo 2) in
  List.length (List.filter (fun x -> Card.rank x = rank_mid) combo) <> 3

let is_one_pair combo = List.length (List.sort_uniq Card.compare combo) = 4
let is_straight_flush combo = is_straight combo && is_flush combo

let is_royal_flush combo =
  is_straight_flush combo && Card.rank_int_of_card (List.hd combo) = 10

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

(* inline tests *)
let%test "test1" =
  combo_value
    (check_combo
       [
         (Card.Hearts, Card.Ten);
         (Card.Hearts, Card.Jack);
         (Card.Hearts, Card.Queen);
         (Card.Hearts, Card.King);
         (Card.Hearts, Card.Ace);
       ])
  = 10

let%test "to_list_test" =
  let cards =
    [
      (Card.Hearts, Card.Two);
      (Card.Hearts, Card.Three);
      (Card.Hearts, Card.Four);
      (Card.Hearts, Card.Five);
      (Card.Hearts, Card.Six);
    ]
  in
  let combo = new_combo cards in
  to_list combo = cards

let%test "to_string_test" =
  let cards =
    [
      (Card.Hearts, Card.Two);
      (Card.Hearts, Card.Three);
      (Card.Hearts, Card.Four);
      (Card.Hearts, Card.Five);
      (Card.Hearts, Card.Six);
    ]
  in
  let combo = new_combo cards in
  to_string combo = "[2♡; 3♡; 4♡; 5♡; 6♡]"
