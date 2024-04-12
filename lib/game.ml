type deck = Deck.t
type player = Player.t
type card = PokerCard.t

type t = {
  deck : deck;
  players : player list;
  pot : int;
  current_bet : int;
  community_cards : card list;
}

let init n =
  {
    deck = Deck.init |> Deck.shuffle;
    players =
      List.init n (fun i ->
          Player.{ id = i; hand = []; chips = 100; is_fold = false });
    pot = 0;
    current_bet = 0;
    community_cards = [];
  }

let deal game =
  let updated_deck, new_players =
    List.fold_left
      (fun (acc_deck, acc_players) p ->
        let card, new_deck = Deck.draw acc_deck in
        (new_deck, Player.add_to_hand p card :: acc_players))
      (game.deck, []) game.players
  in
  { game with deck = updated_deck; players = List.rev new_players }

let deal_community game =
  let card, new_deck = Deck.draw game.deck in
  { game with deck = new_deck; community_cards = card :: game.community_cards }

let bet game player chips =
  {
    game with
    players =
      List.map
        (fun p -> if p = player then Player.decr_chips p chips else p)
        game.players;
    pot = game.pot + chips;
    current_bet = chips;
  }

let all_in game player = bet game player player.chips
let check game player = bet game player 0
let call game player = bet game player game.current_bet

let raise game player chips =
  if chips < game.current_bet then
    failwith "Raise must be greater than current bet"
  else
    {
      game with
      players =
        List.map
          (fun p -> if p = player then Player.decr_chips p chips else p)
          game.players;
      pot = game.pot + chips;
      current_bet = chips;
    }

let fold game player =
  {
    game with
    players =
      List.map
        (fun p -> if p = player then Player.incr_chips p game.pot else p)
        game.players;
  }

let action game player action ?(chips = 0) () =
  match action with
  | "check" -> check game player
  | "call" -> call game player
  | "raise" -> raise game player chips
  | "fold" -> fold game player
  | _ -> failwith "Invalid action"

let turn game player action_str chips =
  match action_str with
  | "raise" -> action game player action_str ~chips ()
  | "check" | "call" | "fold" -> action game player action_str ()
  | _ -> failwith "Invalid action"

let determine_player_action player game =
  if player = List.hd game.players then
    (* User input for the first player *)
    let () = print_endline "Enter your action (check, call, raise, fold): " in
    let action_str = read_line () in
    let chips =
      match action_str with
      | "raise" ->
          print_endline "Enter the number of chips: ";
          int_of_string (read_line ())
      | _ -> 0
    in
    (action_str, chips)
  else
    (* AI decision for other players *)
    (* Placeholder logic - replace with actual AI decision-making *)
    ("call", 0)

let bet_round game =
  List.fold_left
    (fun current_game player ->
      let action_str, chips = determine_player_action player current_game in
      turn current_game player action_str chips)
    game game.players

let to_string game =
  Printf.sprintf "Community Cards: %s\nPot: %d\nCurrent bet: %d\nMy Cards: %s"
    (String.concat "," (List.map PokerCard.to_string game.community_cards))
    game.pot game.current_bet
    (String.concat ","
       (List.map PokerCard.to_string (List.hd game.players).hand))

let player_best_combo game =
  Combo.best_combo (game.community_cards @ (List.hd game.players).hand)
