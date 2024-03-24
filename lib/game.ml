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
end

module type Game = sig
  type deck
  type player
  type card
  type t

  val init : t
  val deal : t -> t
  val bet : t -> player -> int -> t
  val fold : t -> player -> t
  val to_string : t -> string
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
    current_player : player;
  }

  let init =
    {
      deck = D.init |> D.shuffle;
      players = [];
      pot = 0;
      current_bet = 0;
      current_player = { id = 0; hand = []; chips = 0; is_fold = false };
    }

  let deal game =
    let updated_deck, new_players =
      List.fold_left
        (fun (d, acc_players) p ->
          let card, new_deck = D.draw d in
          (new_deck, P.add_to_hand p card :: acc_players))
        (game.deck, []) game.players
    in
    { game with deck = updated_deck; players = List.rev new_players }

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
        List.map
          (fun p -> if p = player then P.incr_chips p game.pot else p)
          game.players;
    }

  let to_string game =
    Printf.sprintf "Pot: %d\nCurrent bet: %d\nCurrent player: %d" game.pot
      game.current_bet game.current_player.id
end
