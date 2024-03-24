open Camel_flush
module PokerDeck = Deck.Make (PokerCard)
module PokerPlayer = Player.Make (PokerCard)
module PokerGame = Game.Make (PokerCard) (PokerDeck) (PokerPlayer)

let game = PokerGame.init 8
let () = print_endline (PokerGame.to_string game)
let deal_game = PokerGame.deal (PokerGame.deal game)
let () = print_endline (PokerGame.to_string deal_game)
