open Camel_flush

let () = Random.self_init ()

let game =
  Game.init 8 |> Game.deal |> Game.deal |> Game.deal_community
  |> Game.deal_community |> Game.deal_community

let () = print_endline (Game.to_string game)
