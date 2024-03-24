open OUnit2
open Camel_flush
open Card
module PokerDeck = Deck.Make (Card)

let new_deck =
  "[2♣; 3♣; 4♣; 5♣; 6♣; 7♣; 8♣; 9♣; 10♣; J♣; Q♣; K♣; A♣; 2♦; 3♦; 4♦; 5♦; 6♦; \
   7♦; 8♦; 9♦; 10♦; J♦; Q♦; K♦; A♦; 2♥; 3♥; 4♥; 5♥; 6♥; 7♥; 8♥; 9♥; 10♥; J♥; \
   Q♥; K♥; A♥; 2♠; 3♠; 4♠; 5♠; 6♠; 7♠; 8♠; 9♠; 10♠; J♠; Q♠; K♠; A♠]"

let tests =
  [
    ( "empty" >:: fun _ ->
      assert_equal new_deck (PokerDeck.to_string PokerDeck.init) );
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

let test_suite = "set test suite" >::: tests
let _ = run_test_tt_main test_suite
let _ = run_test_tt_main card_suite
