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
    | h :: t -> Card.to_string h ^ (if t = [] then "" else " ") ^ to_string' t
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
  match List.sort_uniq Card.compare_rank combo with
  | [ r1; r2 ] -> Card.rank r1 <> Card.rank r2
  | _ -> false

let is_full_house combo =
  match List.sort_uniq Card.compare_rank combo with
  | [ r1; r2 ] ->
      let middle_rank = Card.rank (List.nth combo 3) in
      Card.rank r1 = middle_rank || Card.rank r2 = middle_rank
  | _ -> false

let is_three_of_a_kind combo =
  match List.sort_uniq Card.compare_rank combo with
  | [ r1; r2; r3 ] ->
      Card.rank r1 <> Card.rank r2 && Card.rank r1 <> Card.rank r3
  | _ -> false

let is_two_pair combo =
  let sorted_ranks = List.sort_uniq Card.compare_rank combo in
  match List.length sorted_ranks with
  | 3 ->
      let counts =
        List.map
          (fun r -> List.length (List.filter (fun (_, rank) -> rank = r) combo))
          (List.map snd sorted_ranks)
      in
      List.mem 1 counts && List.mem 2 counts
  | _ -> false

let is_one_pair combo = List.length (List.sort_uniq Card.compare_rank combo) = 4
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
         (Hearts, Jack);
         (Hearts, Queen);
         (Hearts, King);
         (Hearts, Ace);
       ])
  = 10

let%test "to_list_test" =
  let combo =
    new_combo
      [
        (Card.Hearts, Card.Two);
        (Hearts, Three);
        (Hearts, Four);
        (Hearts, Five);
        (Hearts, Six);
      ]
  in
  to_list combo = combo

let%test "to_string_test" =
  let combo =
    new_combo
      [
        (Card.Hearts, Card.Two);
        (Hearts, Three);
        (Hearts, Four);
        (Hearts, Five);
        (Hearts, Six);
      ]
  in
  to_string combo = "2♥ 3♥ 4♥ 5♥ 6♥"

let%test "test_is_flush" =
  let combo =
    new_combo
      [
        (Card.Hearts, Card.Two);
        (Hearts, Three);
        (Hearts, Four);
        (Hearts, Five);
        (Hearts, Six);
      ]
  in
  is_flush combo

let%test "test_is_straight" =
  let combo =
    new_combo
      [
        (Card.Spades, Card.Two);
        (Hearts, Three);
        (Hearts, Four);
        (Hearts, Five);
        (Hearts, Six);
      ]
  in
  is_straight combo

let%test "test_is_four_of_a_kind" =
  let combo =
    new_combo
      [
        (Card.Hearts, Card.Two);
        (Diamonds, Two);
        (Spades, Two);
        (Clubs, Two);
        (Hearts, Five);
      ]
  in
  is_four_of_a_kind combo

let%test "test_is_full_house" =
  let combo =
    new_combo
      [
        (Card.Hearts, Card.Two);
        (Diamonds, Two);
        (Spades, Two);
        (Hearts, Five);
        (Diamonds, Five);
      ]
  in
  is_full_house combo

let%test "test_is_three_of_a_kind" =
  let combo =
    new_combo
      [
        (Card.Hearts, Card.Two);
        (Diamonds, Two);
        (Spades, Two);
        (Hearts, Four);
        (Diamonds, Five);
      ]
  in
  is_three_of_a_kind combo

let%test "test_is_two_pair" =
  let combo =
    new_combo
      [
        (Card.Hearts, Card.Two);
        (Diamonds, Two);
        (Spades, Four);
        (Hearts, Four);
        (Diamonds, Five);
      ]
  in
  is_two_pair combo

let%test "test_is_one_pair" =
  let combo =
    new_combo
      [
        (Card.Hearts, Card.Two);
        (Diamonds, Two);
        (Spades, Four);
        (Hearts, Five);
        (Diamonds, Seven);
      ]
  in
  is_one_pair combo

let%test "test_check_combo" =
  let combo =
    new_combo
      [
        (Card.Hearts, Card.Two);
        (Hearts, Three);
        (Hearts, Four);
        (Clubs, Five);
        (Spades, Queen);
      ]
  in
  check_combo combo = HighCard
