module type Card = sig
  type t

  val to_string : t -> string
end

module type Deck = sig
  type card
  type t

  val init : t
  val shuffle : t -> t
  val draw : t -> card * t
end

module type Player = sig
  type card

  type t = {
    id : int;
    hand : card list;
    chips : int;
    is_fold : bool;
  }

  val is_fold : t -> bool
  val incr_chips : t -> int -> t
  val decr_chips : t -> int -> t
  val add_to_hand : t -> card -> t
  val fold : t -> t
end

module type Game = sig
  type deck
  type player
  type card
  type t

  val init : int -> t
  val deal : t -> t
  val deal_community : t -> t
  val bet : t -> player -> int -> t
  val fold : t -> player -> t
  val to_string : t -> string
  val get_nth_player : t -> int -> player
  val players : t -> player list
end

module Make
    (C : Card)
    (D : Deck with type card = C.t)
    (P : Player with type card = C.t) :
  Game with type deck = D.t and type player = P.t and type card = C.t = struct
  type deck = D.t
  type player = P.t
  type card = C.t

  type t = {
    deck : deck;
    players : player list;
    pot : int;
    current_bet : int;
    community_cards : card list;
  }

  let init n =
    {
      deck = D.init |> D.shuffle;
      players =
        List.init n (fun i ->
            P.{ id = i; hand = []; chips = 100; is_fold = false });
      pot = 0;
      current_bet = 0;
      community_cards = [];
    }

  let deal game =
    let updated_deck, new_players =
      List.fold_left
        (fun (acc_deck, acc_players) p ->
          let card, new_deck = D.draw acc_deck in
          (new_deck, P.add_to_hand p card :: acc_players))
        (game.deck, []) game.players
    in
    { game with deck = updated_deck; players = List.rev new_players }

  let deal_community game =
    let card, new_deck = D.draw game.deck in
    {
      game with
      deck = new_deck;
      community_cards = card :: game.community_cards;
    }

  let bet game player chips =
    {
      game with
      players =
        List.map
          (fun p -> if p = player then P.decr_chips p chips else p)
          game.players;
      pot = game.pot + chips;
      current_bet = chips;
    }

  let fold game player =
    {
      game with
      players =
        List.map (fun p -> if p = player then P.fold p else p) game.players;
    }

  let to_string game =
    Printf.sprintf
      "Community Cards: %s\nPot: %d\nCurrent bet: %d\nMy Cards: %s\n"
      (String.concat "," (List.map C.to_string game.community_cards))
      game.pot game.current_bet
      (String.concat "," (List.map C.to_string (List.hd game.players).hand))

  let get_nth_player game n = List.nth game.players n
  let players game = game.players
end
