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

let check game player = bet game player 0

let fold game player =
  {
    game with
    players =
      List.map
        (fun p -> if p = player then Player.incr_chips p game.pot else p)
        game.players;
  }

let to_string game =
  Printf.sprintf "Community Cards: %s\nPot: %d\nCurrent bet: %d\nMy Cards: %s"
    (String.concat "," (List.map PokerCard.to_string game.community_cards))
    game.pot game.current_bet
    (String.concat ","
       (List.map PokerCard.to_string (List.hd game.players).hand))
