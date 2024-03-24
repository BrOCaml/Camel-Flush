open OUnit2
open Camel_flush
module PokerDeck = Deck.Make (Card)

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

let test_suite = "set test suite" >::: tests
let _ = run_test_tt_main test_suite
