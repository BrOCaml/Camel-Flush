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

exception Tie

let new_combo (c : PokerCard.t list) : t =
  if List.length c = 5 then List.sort PokerCard.compare c
  else failwith "Invalid combo"

let to_list (c : t) : PokerCard.t list = c

let string_of_combo_type t =
  match t with
  | HighCard -> "High Card"
  | Pair -> "Pair"
  | TwoPair -> "Two Pair"
  | ThreeOfAKind -> "Three of a Kind"
  | Straight -> "Straight"
  | Flush -> "Flush"
  | FullHouse -> "Full House"
  | FourOfAKind -> "Four of a Kind"
  | StraightFlush -> "Straight Flush"
  | RoyalFlush -> "Royal Flush"

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

let rec combinations k list =
  if k = 0 then [ [] ]
  else
    match list with
    | [] -> []
    | h :: tl ->
        let with_h =
          List.map (fun combo -> h :: combo) (combinations (k - 1) tl)
        in
        let without_h = combinations k tl in
        with_h @ without_h

let best_combo card_lst =
  let all_combos = combinations 5 card_lst in
  let best_combo =
    List.fold_left
      (fun acc combo ->
        let combo = new_combo combo in
        let current_combo_type = check_combo combo in
        let acc_combo_type = check_combo acc in
        if combo_value current_combo_type > combo_value acc_combo_type then
          combo
        else acc)
      (List.hd all_combos) all_combos
  in
  to_string best_combo ^ "\nType: "
  ^ string_of_combo_type (check_combo best_combo)

let best_combo_type card_lst =
  let all_combos = combinations 5 card_lst in
  let best_combo =
    List.fold_left
      (fun acc combo ->
        let combo = new_combo combo in
        let current_combo_type = check_combo combo in
        let acc_combo_type = check_combo acc in
        if combo_value current_combo_type > combo_value acc_combo_type then
          combo
        else acc)
      (List.hd all_combos) all_combos
  in
  check_combo best_combo

(* c1 and c2 are combos, 1 for c1 -1 for c2 0 for tie *)
let higher_royalflush c1 c2 =
  let s1 = PokerCard.suit (List.nth (to_list c1) 0) in
  let s2 = PokerCard.suit (List.nth (to_list c2) 0) in
  if s1 > s2 then 1 else if s2 > s1 then -1 else 0

(* for striaght and straight flush *)
let higher_straight_flush c1 c2 =
  let max_rank_hand1 = PokerCard.rank_int_of_card (List.nth (to_list c1) 4) in
  let max_rank_hand2 = PokerCard.rank_int_of_card (List.nth (to_list c2) 4) in
  if max_rank_hand1 > max_rank_hand2 then 1
  else if max_rank_hand1 < max_rank_hand2 then -1
  else 0

let higher_fullhouse c1 c2 =
  let rank_list1 = List.sort PokerCard.compare_rank (to_list c1) in
  let rank_list2 = List.sort PokerCard.compare_rank (to_list c2) in
  let rec find_three_rank = function
    | [] -> None (* not possible *)
    | c :: rest -> (
        match rest with
        | a :: b :: _
          when PokerCard.rank a = PokerCard.rank c
               && PokerCard.rank b = PokerCard.rank c ->
            Some (PokerCard.rank_int_of_card c)
        | _ :: _ :: rest' -> find_three_rank rest'
        | _ -> None)
  in
  match (find_three_rank rank_list1, find_three_rank rank_list2) with
  | Some rank1, Some rank2 ->
      if rank1 > rank2 then 1
      else if rank1 < rank2 then -1
      else 0 (* ranks are equal *)
  | Some _, None -> 1
  | None, Some _ -> -1
  | None, None -> 0

let rec last = function
  | [] -> failwith "empty_list"
  | [ x ] -> x
  | _ :: t -> last t

let higher_fourofakind c1 c2 =
  let sorted_h1 = List.sort PokerCard.compare_rank (to_list c1) in
  let sorted_h2 = List.sort PokerCard.compare_rank (to_list c2) in
  let rank_hand1 =
    List.hd
      (List.sort_uniq compare (List.map PokerCard.rank_int_of_card sorted_h1))
  in
  let rank_hand2 =
    List.hd
      (List.sort_uniq compare (List.map PokerCard.rank_int_of_card sorted_h2))
  in
  if rank_hand1 > rank_hand2 then 1
  else if rank_hand1 < rank_hand2 then -1
  else
    let kicker_h1 = last sorted_h1 |> PokerCard.rank_int_of_card in
    let kicker_h2 = last sorted_h2 |> PokerCard.rank_int_of_card in

    if kicker_h1 > kicker_h2 then 1 else if kicker_h1 < kicker_h2 then -1 else 0

let higher_flush c1 c2 =
  let max_rank_hand1 = PokerCard.rank_int_of_card (List.nth (to_list c1) 4) in
  let max_rank_hand2 = PokerCard.rank_int_of_card (List.nth (to_list c2) 4) in
  if max_rank_hand1 > max_rank_hand2 then 1
  else if max_rank_hand1 < max_rank_hand2 then -1
  else 0

let best_three_of_a_kind cards =
  let sorted_cards = List.sort PokerCard.compare_rank cards in
  let rec find_three_of_a_kind = function
    | c1 :: c2 :: c3 :: rest
      when PokerCard.rank c1 = PokerCard.rank c2
           && PokerCard.rank c2 = PokerCard.rank c3 ->
        Some
          ( PokerCard.rank_int_of_card c1,
            List.filter (fun c -> PokerCard.rank c <> PokerCard.rank c1) rest )
    | _ :: rest -> find_three_of_a_kind rest
    | [] -> None
  in
  find_three_of_a_kind sorted_cards

let higher_three_of_a_kind c1 c2 =
  let sorted_hand1 = List.sort PokerCard.compare_rank (to_list c1) in
  let sorted_hand2 = List.sort PokerCard.compare_rank (to_list c2) in
  let rank_three1 =
    match best_three_of_a_kind sorted_hand1 with
    | Some (rank, _) -> rank
    | None -> failwith "Invalid three of a kind"
  in
  let rank_three2 =
    match best_three_of_a_kind sorted_hand2 with
    | Some (rank, _) -> rank
    | None -> failwith "Invalid three of a kind"
  in
  compare rank_three1 rank_three2

let extract_pairs_and_kicker hand =
  let sorted_hand = List.sort PokerCard.compare_rank hand in
  let rec aux_pairs_and_kicker pairs rest =
    match rest with
    | [] -> pairs
    | card :: remaining_rest -> (
        let pair_rank = PokerCard.rank_int_of_card card in
        match pairs with
        | [] -> aux_pairs_and_kicker [ (pair_rank, card) ] remaining_rest
        | (prev_rank, _) :: _ ->
            if prev_rank = pair_rank then
              aux_pairs_and_kicker ((pair_rank, card) :: pairs) remaining_rest
            else pairs)
  in
  let pairs_and_kicker = aux_pairs_and_kicker [] sorted_hand in
  match pairs_and_kicker with
  | [] -> None
  | (pair1_rank, kicker1) :: [] ->
      Some (pair1_rank, 0, PokerCard.rank_int_of_card kicker1)
  | (pair1_rank, kicker1) :: (pair2_rank, _) :: _ ->
      Some (pair1_rank, pair2_rank, PokerCard.rank_int_of_card kicker1)

let higher_two_pair c1 c2 =
  let sorted_hand1 = List.sort PokerCard.compare_rank (to_list c1) in
  let sorted_hand2 = List.sort PokerCard.compare_rank (to_list c2) in
  match
    ( extract_pairs_and_kicker sorted_hand1,
      extract_pairs_and_kicker sorted_hand2 )
  with
  | ( Some (pair1_rank1, pair2_rank1, kicker1),
      Some (pair1_rank2, pair2_rank2, kicker2) ) ->
      let compare_higher_pairs = compare pair1_rank1 pair1_rank2 in
      if compare_higher_pairs <> 0 then compare_higher_pairs
      else
        let compare_lower_pairs = compare pair2_rank1 pair2_rank2 in
        if compare_lower_pairs <> 0 then compare_lower_pairs
        else
          (* compare the kickers if both players have the same two pairs *)
          let compare_kickers = compare kicker1 kicker2 in
          if compare_kickers <> 0 then compare_kickers else 0
  | _ -> failwith "Invalid hand"

let higher_pair c1 c2 =
  let sorted_hand1 = List.sort PokerCard.compare_rank (to_list c1) in
  let sorted_hand2 = List.sort PokerCard.compare_rank (to_list c2) in
  match
    ( extract_pairs_and_kicker sorted_hand1,
      extract_pairs_and_kicker sorted_hand2 )
  with
  | Some (pair1_rank1, _, _), Some (pair1_rank2, _, _) ->
      let compare_pairs = compare pair1_rank1 pair1_rank2 in
      if compare_pairs <> 0 then compare_pairs
      else
        let rec compare_remaining_cards hand1 hand2 =
          match (hand1, hand2) with
          | [], [] -> 0 (* same hands *)
          | [], _ -> -1
          | _, [] -> 1
          | card1 :: rest1, card2 :: rest2 ->
              let rank1 = PokerCard.rank_int_of_card card1 in
              let rank2 = PokerCard.rank_int_of_card card2 in
              if rank1 <> rank2 then compare rank1 rank2
              else compare_remaining_cards rest1 rest2
        in
        compare_remaining_cards (List.tl sorted_hand1) (List.tl sorted_hand2)
  | _ -> failwith "Invalid hand"

let higher_high_card c1 c2 =
  let c1_lst = to_list c1 in
  let c2_lst = to_list c2 in
  if List.length c1_lst < 5 || List.length c2_lst < 5 then
    failwith "Invalid hand <5"
  else
    let c1_first_rank = PokerCard.rank_int_of_card (List.nth c1_lst 0) in
    let c2_first_rank = PokerCard.rank_int_of_card (List.nth c2_lst 0) in
    let c1_second_rank = PokerCard.rank_int_of_card (List.nth c1_lst 1) in
    let c2_second_rank = PokerCard.rank_int_of_card (List.nth c2_lst 1) in
    let c1_third_rank = PokerCard.rank_int_of_card (List.nth c1_lst 2) in
    let c2_third_rank = PokerCard.rank_int_of_card (List.nth c2_lst 2) in
    let c1_fourth_rank = PokerCard.rank_int_of_card (List.nth c1_lst 3) in
    let c2_fourth_rank = PokerCard.rank_int_of_card (List.nth c2_lst 3) in
    let c1_fifth_rank = PokerCard.rank_int_of_card (List.nth c1_lst 4) in
    let c2_fifth_rank = PokerCard.rank_int_of_card (List.nth c2_lst 4) in
    let fst_c1 = List.nth c1_lst 0 in
    let fst_c2 = List.nth c2_lst 0 in
    if fst_c1 = fst_c2 then
      if c1_first_rank = c2_first_rank then
        if c1_second_rank = c2_second_rank then
          if c1_third_rank = c2_third_rank then
            if c1_fourth_rank = c2_fourth_rank then
              if c1_fifth_rank = c2_fifth_rank then raise Tie
              else c1_fifth_rank > c2_fifth_rank
            else c1_fourth_rank > c2_fourth_rank
          else c1_third_rank > c2_third_rank
        else c1_second_rank > c2_second_rank
      else c1_first_rank > c2_first_rank
    else fst_c1 > fst_c2

let compare_hands card_list1 card_list2 =
  let v1 = combo_value (best_combo_type card_list1) in
  let v2 = combo_value (best_combo_type card_list2) in
  if v1 > v2 then 1
  else if v1 < v2 then -1
  else
    match best_combo_type card_list1 with
    | RoyalFlush -> higher_royalflush card_list1 card_list2
    | StraightFlush -> higher_straight_flush card_list1 card_list2
    | FourOfAKind -> higher_fourofakind card_list1 card_list2
    | FullHouse -> higher_fullhouse card_list1 card_list2
    | Flush -> higher_flush card_list1 card_list2
    | Straight -> higher_straight_flush card_list1 card_list2
    | ThreeOfAKind -> higher_three_of_a_kind card_list1 card_list2
    | TwoPair -> higher_two_pair card_list1 card_list2
    | Pair -> higher_pair card_list1 card_list2
    | HighCard -> (
        match higher_high_card card_list1 card_list2 with
        | true -> 1
        | false -> -1
        | exception Tie -> 0)
