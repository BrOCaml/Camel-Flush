open Camel_flush

let () = Random.self_init ()

let game =
  Game.init 8 |> Game.deal |> Game.deal |> Game.deal_community
  |> Game.deal_community |> Game.deal_community

(* Greet *)
let () =
  print_endline "Welcome to Camel Flush!\nWhat's your name?";
  flush stdout;
  let name = read_line () in
  print_endline ("Hello " ^ name ^ "!\n" ^ "Let's play")

(* let user = PokerGame.get_nth_player game 0 *)
let () = print_endline (Game.to_string game)

let () =
  (* First round *)
  let game = Game.bet_round game in
  let game = Game.deal_community game in
  print_endline (Game.to_string game);

  (* Second round *)
  let game = Game.bet_round game in
  let game = Game.deal_community game in
  print_endline (Game.to_string game);

  (* User best combo *)
  print_endline ("Your Best Combo is: \n" ^ Game.player_best_combo game);

  (* AI best combo *)
  print_endline "AI's Best Combos: ";
  Game.print_best_combos game;

  (* Find winner *)
  let winner = Game.determine_winner game in
  if winner = "Tie" then print_endline "It's a Tie!"
  else if winner = "Player 0" then print_endline "You Win!"
  else print_endline ("Winner: " ^ Game.determine_winner game)
