type t = Card.t list
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
       combo)

let is_four_of_a_kind combo =
  let first_four = List.tl (List.rev combo) in
  let last_four = List.tl combo in
  let same = List.for_all (fun x -> Card.rank x = Card.rank (List.hd combo)) in
  same first_four || same last_four

let is_full_house combo =
  if List.length (List.sort_uniq Card.compare_rank combo) = 2 then
    let count1 = List.length (List.filter (fun x -> x = List.hd combo) combo) in
    let count2 =
      List.length (List.filter (fun x -> x = List.nth combo 1) combo)
    in
    (count1 = 2 && count2 = 3) || (count1 = 3 && count2 = 2)
  else false

let is_two_pair combo =
  if List.length (List.sort_uniq Card.compare combo) = 3 then
    let count1 = List.length (List.filter (fun x -> x = List.hd combo) combo) in
    let count2 =
      List.length (List.filter (fun x -> x = List.nth combo 1) combo)
    in
    let count3 =
      List.length (List.filter (fun x -> x = List.nth combo 2) combo)
    in
    (count1 = 2 && count2 = 2)
    || (count1 = 2 && count3 = 2)
    || (count2 = 2 && count3 = 2)
  else false

let is_three_of_a_kind combo =
  if List.length (List.sort_uniq Card.compare combo) = 3 then
    let count1 = List.length (List.filter (fun x -> x = List.hd combo) combo) in
    let count2 =
      List.length (List.filter (fun x -> x = List.nth combo 1) combo)
    in
    let count3 =
      List.length (List.filter (fun x -> x = List.nth combo 2) combo)
    in
    count1 = 3 || count2 = 3 || count3 = 3
  else false

let is_straight_flush combo = is_straight combo && is_flush combo

let is_royal_flush combo =
  is_straight_flush combo && Card.rank_int_of_card (List.hd combo) = 10
