open Camel_flush

let () = Random.self_init ()

let game =
  Game.init 8 |> Game.deal |> Game.deal |> Game.deal_community
  |> Game.deal_community |> Game.deal_community

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

  print_endline ("Your Best Combo is: " ^ Game.player_best_combo game)
