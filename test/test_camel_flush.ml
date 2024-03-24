open OUnit2
open Camel_flush
open PokerCard
module PokerDeck = Deck.Make (PokerCard)
module Player = Player.Make (PokerCard)

let new_deck =
  "[2♣; 3♣; 4♣; 5♣; 6♣; 7♣; 8♣; 9♣; 10♣; J♣; Q♣; K♣; A♣; 2♦; 3♦; 4♦; 5♦; 6♦; \
   7♦; 8♦; 9♦; 10♦; J♦; Q♦; K♦; A♦; 2♥; 3♥; 4♥; 5♥; 6♥; 7♥; 8♥; 9♥; 10♥; J♥; \
   Q♥; K♥; A♥; 2♠; 3♠; 4♠; 5♠; 6♠; 7♠; 8♠; 9♠; 10♠; J♠; Q♠; K♠; A♠]"

let tests =
  [
    ( "test_empty" >:: fun _ ->
      assert_equal new_deck (PokerDeck.to_string PokerDeck.init) );
    ( "test_valid_combo_length" >:: fun _ ->
      let new_combo =
        Combo.new_combo
          [
            (Hearts, Two);
            (Hearts, Three);
            (Hearts, Four);
            (Hearts, Five);
            (Hearts, Six);
          ]
      in
      assert_equal 5 (List.length (Combo.to_list new_combo)) );
    ( "test_invalid_combo_length" >:: fun _ ->
      assert_raises (Failure "Invalid combo") (fun () ->
          Combo.new_combo [ (Hearts, Two); (Hearts, Three); (Hearts, Four) ]) );
  ]

(*card test*)
let test_rank_to_int _ =
  assert_equal (rank_to_int Two) 2;
  assert_equal (rank_to_int Ace) 14

let test_compare _ =
  assert_equal (compare (Hearts, Two) (Hearts, Three)) (-1);
  assert_equal (compare (Hearts, Three) (Hearts, Two)) 1;
  assert_equal (compare (Hearts, Two) (Hearts, Two)) 0

let test_compare_rank _ =
  assert_equal (compare_rank (Hearts, Two) (Hearts, Three)) (-1);
  assert_equal (compare_rank (Hearts, Three) (Hearts, Two)) 1;
  assert_equal (compare_rank (Hearts, Two) (Hearts, Two)) 0

let test_to_string _ =
  assert_equal (to_string (Hearts, Two)) "2♥";
  assert_equal (to_string (Spades, Ace)) "A♠"

let test_suit _ =
  assert_equal (suit (Hearts, Two)) Hearts;
  assert_equal (suit (Spades, Ace)) Spades

let test_rank _ =
  assert_equal (rank (Hearts, Two)) Two;
  assert_equal (rank (Spades, Ace)) Ace

let test_create_deck _ = assert_equal (List.length create_deck) 52

let test_rank_int_of_card _ =
  assert_equal (rank_int_of_card (Hearts, Two)) 2;
  assert_equal (rank_int_of_card (Spades, Ace)) 14

let card_suite =
  "CardTestSuite"
  >::: [
         "test_rank_to_int" >:: test_rank_to_int;
         "test_compare" >:: test_compare;
         "test_compare_rank" >:: test_compare_rank;
         "test_to_string" >:: test_to_string;
         "test_suit" >:: test_suit;
         "test_rank" >:: test_rank;
         "test_create_deck" >:: test_create_deck;
         "test_rank_int_of_card" >:: test_rank_int_of_card;
       ]

let test_combo_map cards f _ = assert_bool (Combo.to_string cards) (f cards)

let test_new_combo_sort _ =
  let cards =
    [
      (Hearts, Two);
      (Hearts, King);
      (Hearts, Four);
      (Hearts, Eight);
      (Hearts, Six);
    ]
  in
  let sorted_cards =
    [
      (Hearts, Two);
      (Hearts, Four);
      (Hearts, Six);
      (Hearts, Eight);
      (Hearts, King);
    ]
  in
  assert_equal sorted_cards (Combo.to_list (Combo.new_combo cards))

let test_is_flush =
  let cards =
    [
      (Hearts, Two);
      (Hearts, Four);
      (Hearts, Six);
      (Hearts, Eight);
      (Hearts, King);
    ]
  in
  test_combo_map (Combo.new_combo cards) Combo.is_flush

let test_is_flush_fail _ =
  let cards =
    [
      (Hearts, Two);
      (Hearts, Four);
      (Hearts, Six);
      (Hearts, Eight);
      (Spades, King);
    ]
  in
  assert_bool "flush fail" (not (Combo.is_flush (Combo.new_combo cards)))

let test_is_straight =
  let cards =
    [
      (Hearts, Two);
      (Diamonds, Three);
      (Hearts, Four);
      (Clubs, Five);
      (Spades, Six);
    ]
  in
  test_combo_map (Combo.new_combo cards) Combo.is_straight

let test_is_straight_fail _ =
  let cards =
    [
      (Hearts, Two);
      (Diamonds, Three);
      (Hearts, Four);
      (Clubs, Five);
      (Spades, Seven);
    ]
  in
  assert_bool "straight fail" (not (Combo.is_straight (Combo.new_combo cards)))

let test_is_straight_flush =
  let cards =
    [
      (Hearts, Two);
      (Hearts, Three);
      (Hearts, Four);
      (Hearts, Five);
      (Hearts, Six);
    ]
  in
  test_combo_map (Combo.new_combo cards) Combo.is_straight_flush

let test_is_straight_flush_fail _ =
  let cards =
    [
      (Hearts, Two);
      (Hearts, Three);
      (Hearts, Four);
      (Hearts, Five);
      (Hearts, Seven);
    ]
  in
  assert_bool "straight flush fail"
    (not (Combo.is_straight_flush (Combo.new_combo cards)))

let test_is_royal_flush =
  let cards =
    [
      (Hearts, Ten);
      (Hearts, Jack);
      (Hearts, Queen);
      (Hearts, King);
      (Hearts, Ace);
    ]
  in
  test_combo_map (Combo.new_combo cards) Combo.is_royal_flush

let test_is_royal_flush_fail _ =
  let cards =
    [
      (Hearts, Ten);
      (Hearts, Jack);
      (Hearts, Queen);
      (Hearts, King);
      (Hearts, Two);
    ]
  in
  assert_bool "royal flush fail"
    (not (Combo.is_royal_flush (Combo.new_combo cards)))

let test_is_four_of_a_kind =
  let cards =
    [
      (Hearts, Two);
      (Diamonds, Two);
      (Clubs, Two);
      (Spades, Two);
      (Hearts, Three);
    ]
  in
  test_combo_map (Combo.new_combo cards) Combo.is_four_of_a_kind

let test_is_four_of_a_kind_two =
  let cards =
    [
      (Hearts, Two);
      (Diamonds, Three);
      (Clubs, Three);
      (Spades, Three);
      (Hearts, Three);
    ]
  in
  test_combo_map (Combo.new_combo cards) Combo.is_four_of_a_kind

let test_is_four_of_a_kind_fail _ =
  let cards =
    [
      (Hearts, Two);
      (Diamonds, Two);
      (Clubs, Two);
      (Spades, Three);
      (Hearts, Three);
    ]
  in
  assert_bool "four of a kind fail"
    (not (Combo.is_four_of_a_kind (Combo.new_combo cards)))

let test_is_full_house =
  let cards =
    [
      (Hearts, Two);
      (Diamonds, Two);
      (Clubs, Two);
      (Spades, Three);
      (Hearts, Three);
    ]
  in
  test_combo_map (Combo.new_combo cards) Combo.is_full_house

let test_is_three_of_a_kind =
  let cards =
    [
      (Hearts, Two);
      (Diamonds, Two);
      (Clubs, Two);
      (Spades, Three);
      (Hearts, Four);
    ]
  in
  test_combo_map (Combo.new_combo cards) Combo.is_three_of_a_kind

let test_is_three_of_a_kind_two =
  let cards =
    [
      (Hearts, Two);
      (Diamonds, Three);
      (Clubs, Three);
      (Spades, Three);
      (Hearts, Four);
    ]
  in
  test_combo_map (Combo.new_combo cards) Combo.is_three_of_a_kind

let test_is_three_of_a_kind_three =
  let cards =
    [
      (Hearts, Two);
      (Diamonds, Three);
      (Clubs, Four);
      (Spades, Four);
      (Hearts, Four);
    ]
  in
  test_combo_map (Combo.new_combo cards) Combo.is_three_of_a_kind

let test_is_three_of_a_kind_fail _ =
  let cards =
    [
      (Hearts, Two);
      (Diamonds, Two);
      (Clubs, Three);
      (Spades, Three);
      (Hearts, Four);
    ]
  in
  assert_bool "three of a kind fail"
    (not (Combo.is_three_of_a_kind (Combo.new_combo cards)))

let test_is_two_pair =
  let cards =
    [
      (Hearts, Two);
      (Diamonds, Two);
      (Clubs, Three);
      (Spades, Three);
      (Hearts, Four);
    ]
  in
  test_combo_map (Combo.new_combo cards) Combo.is_two_pair

let test_is_two_pair_fail _ =
  let cards =
    [
      (Hearts, Two);
      (Diamonds, Two);
      (Clubs, Three);
      (Spades, Four);
      (Hearts, Five);
    ]
  in
  assert_bool "two pair fail" (not (Combo.is_two_pair (Combo.new_combo cards)))

let test_is_one_pair =
  let cards =
    [
      (Hearts, Two);
      (Diamonds, Two);
      (Clubs, Three);
      (Spades, Four);
      (Hearts, Five);
    ]
  in
  test_combo_map (Combo.new_combo cards) Combo.is_one_pair

let test_is_one_pair_fail _ =
  let cards =
    [
      (Hearts, Two);
      (Diamonds, Three);
      (Clubs, Four);
      (Spades, Five);
      (Hearts, Six);
    ]
  in
  assert_bool "pair fail" (not (Combo.is_one_pair (Combo.new_combo cards)))

let combo_test_suite =
  "card combo test suite"
  >::: [
         "test_new_combo_sort" >:: test_new_combo_sort;
         "test_is_flush" >:: test_is_flush;
         "test_is_flush_fail" >:: test_is_flush_fail;
         "test_is_straight" >:: test_is_straight;
         "test_is_straight_fail" >:: test_is_straight_fail;
         "test_is_straight_flush" >:: test_is_straight_flush;
         "test_is_straight_flush_fail" >:: test_is_straight_flush_fail;
         "test_is_royal_flush" >:: test_is_royal_flush;
         "test_is_royal_flush_fail" >:: test_is_royal_flush_fail;
         "test_is_four_of_a_kind" >:: test_is_four_of_a_kind;
         "test_is_four_of_a_kind_two" >:: test_is_four_of_a_kind_two;
         "test_is_four_of_a_kind_fail" >:: test_is_four_of_a_kind_fail;
         "test_is_full_house" >:: test_is_full_house;
         "test_is_three_of_a_kind" >:: test_is_three_of_a_kind;
         "test_is_three_of_a_kind_two" >:: test_is_three_of_a_kind_two;
         "test_is_three_of_a_kind_three" >:: test_is_three_of_a_kind_three;
         "test_is_three_of_a_kind_fail" >:: test_is_three_of_a_kind_fail;
         "test_is_two_pair" >:: test_is_two_pair;
         "test_is_two_pair_fail" >:: test_is_two_pair_fail;
         "test_is_one_pair" >:: test_is_one_pair;
         "test_is_pair_fail" >:: test_is_one_pair_fail;
       ]

let player = Player.create 0 [] 100
let test_player_id _ = assert_equal player.id 0
let test_player_hand _ = assert_equal player.hand []
let test_player_chips _ = assert_equal player.chips 100
let test_player_fold _ = assert_equal player.is_fold false
let test_incr_chips _ = assert_equal (Player.incr_chips player 100).chips 200
let test_decr_chips _ = assert_equal (Player.decr_chips player 50).chips 50

let test_add_to_hand _ =
  assert_equal (Player.add_to_hand player (Hearts, Two)).hand [ (Hearts, Two) ]

let player_suite =
  "PlayerTestSuite"
  >::: [
         "test_player_id" >:: test_player_id;
         "test_player_hand" >:: test_player_hand;
         "test_player_chips" >:: test_player_chips;
         "test_player_fold" >:: test_player_fold;
         "test_incr_chips" >:: test_incr_chips;
         "test_decr_chips" >:: test_decr_chips;
         "test_add_to_hand" >:: test_add_to_hand;
       ]

let test_suite = "set test suite" >::: tests
let _ = run_test_tt_main test_suite
let _ = run_test_tt_main card_suite
let _ = run_test_tt_main combo_test_suite
let _ = run_test_tt_main player_suite
