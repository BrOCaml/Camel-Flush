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

let raise game (player : player) chips =
  if chips < game.current_bet then
    failwith "Raise must be greater than current bet"
    (* player cannot raise more chips than their chips *)
  else if chips > player.chips then all_in game player
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
  Printf.sprintf
    "Community Cards: %s\nPot: %d\nCurrent Bet: %d\nMy Cards: %s\nMy Chips: %d"
    (String.concat " " (List.map PokerCard.to_string game.community_cards))
    game.pot game.current_bet
    (String.concat " "
       (List.map PokerCard.to_string (List.hd game.players).hand))
    (List.hd game.players).chips

let player_best_combo game =
  Combo.best_combo (game.community_cards @ (List.hd game.players).hand)

let print_best_combos game =
  List.iter
    (fun player ->
      let best_combo =
        Combo.best_combo (game.community_cards @ (player : player).hand)
      in
      print_endline ("Player " ^ string_of_int player.id ^ ":\n" ^ best_combo))
    (List.tl game.players)

let check_higher_hand c1 c2 =
  let v = Combo.compare_hands c1 c2 in
  if v = 1 then Some c1 else if v = -1 then Some c2 else None

let determine_winner game =
  match game.players with
  | [] -> "No players in the game"
  | [ player ] -> Player.to_string player
  | player :: players ->
      let rec find_winner (players : player list) (current_winner : player)
          (tied_players : player list) =
        let current_winner_cards = game.community_cards @ current_winner.hand in
        match players with
        | [] ->
            if List.length tied_players = 0 then
              Player.to_string_name current_winner
            else "Tie"
        | player :: rest -> (
            let player_cards = game.community_cards @ player.hand in
            match check_higher_hand player_cards current_winner_cards with
            | Some cards ->
                if cards = current_winner_cards then
                  find_winner rest current_winner []
                else find_winner rest player []
            | None -> find_winner rest current_winner (player :: tied_players))
      in
      let initial_winner = player in
      find_winner players initial_winner []
