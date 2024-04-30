(** @authors: Mohamed Katary (mtk67), Frank Dai (sd924), Huajie Zhong (hz642) *)

open Camel_flush

let () = Random.self_init ()
let game = Game.init 8 |> Game.deal |> Game.deal

(* Greet *)
let () =
  print_endline "Welcome to Camel Flush!\nWhat's your name?";
  flush stdout;
  let name = read_line () in
  print_endline ("Hello " ^ name ^ ", " ^ "let's play!\n")

(* let user = PokerGame.get_nth_player game 0 *)

let () =
  (* First round *)
  print_endline "---First Round---";
  print_endline (Game.to_string game);
  let game = Game.bet_round game in

  (* Second round *)
  print_endline "---Second Round---";
  let game = Game.deal_community game in
  let game = Game.deal_community game in
  let game = Game.deal_community game in
  print_endline (Game.to_string game);
  let game = Game.bet_round game in

  (* Third round *)
  print_endline "---Third Round---";
  let game = Game.deal_community game in
  print_endline (Game.to_string game);
  let game = Game.bet_round game in

  (* Fourth round *)
  print_endline "---Fourth Round---";
  let game = Game.deal_community game in
  print_endline (Game.to_string game);
  let game = Game.bet_round game in

  (* User best combo *)
  print_endline ("Your Best Combo is: \n" ^ Game.player_best_combo game);

  (* AI best combos *)
  print_endline "AI's Best Combos: ";
  Game.print_best_combos game;

  (* Find winner *)
  let winner = Game.determine_winner game in
  if winner = "Tie" then print_endline "It's a Tie!"
  else if winner = "Player 0" then
    print_endline ("You win " ^ string_of_int (Game.get_pot game) ^ " chips!")
  else
    let winner = Game.determine_winner game in
    if winner = "No players in the game" then
      print_endline "No players in the game."
    else
      print_endline
        (winner ^ " wins " ^ string_of_int (Game.get_pot game) ^ " chips!")

(* Print game *)
