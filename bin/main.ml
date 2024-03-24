open Camel_flush
module PokerDeck = Deck.Make (PokerCard)
module PokerPlayer = Player.Make (PokerCard)
module PokerGame = Game.Make (PokerCard) (PokerDeck) (PokerPlayer)

let game =
  PokerGame.init 8 |> PokerGame.deal |> PokerGame.deal
  |> PokerGame.deal_community |> PokerGame.deal_community
  |> PokerGame.deal_community

let () = print_endline (PokerGame.to_string game)
