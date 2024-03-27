open Camel_flush
module PokerDeck = Deck.Make (PokerCard)
module PokerPlayer = Player.Make (PokerCard)
module PokerGame = Game.Make (PokerCard) (PokerDeck) (PokerPlayer)

let game =
  PokerGame.init 8 |> PokerGame.deal |> PokerGame.deal
  |> PokerGame.deal_community |> PokerGame.deal_community
  |> PokerGame.deal_community

(* let user = PokerGame.get_nth_player game 0 *)
let () = print_endline (PokerGame.to_string game)
let _ = print_endline "It's your turn\nDo you want to fold, call, or raise?"

let decision =
  let rec ask () =
    let line = String.uppercase_ascii (read_line ()) in
    if line <> "FOLD" && line <> "CALL" && line <> "RAISE" then begin
      print_endline "Invalid input. Please enter fold, call, or raise.";
      ask ()
    end
    else line
  in
  String.capitalize_ascii (String.lowercase_ascii (ask ()))

let () = print_endline ("You chose: " ^ decision)
