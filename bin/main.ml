open Camel_flush
module PokerDeck = Deck.Make (PokerCard)
module PokerPlayer = Player.Make (PokerCard)
module PokerGame = Game.Make (PokerCard) (PokerDeck) (PokerPlayer)

let game = PokerGame.init
let () = print_string (PokerGame.to_string game)
