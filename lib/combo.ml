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
