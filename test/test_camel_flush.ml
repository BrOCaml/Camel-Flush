open OUnit2
open Camel_flush
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

let test_suite = "set test suite" >::: tests
let _ = run_test_tt_main test_suite
