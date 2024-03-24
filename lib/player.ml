module type Card = sig
  type t

  val create_deck : t list
  val to_string : t -> string
end

module type Player = sig
  type t
  type card

  val create : int -> card list -> int -> t
  val incr_chips : t -> int -> t

  val to_string : t -> string
  val to_string_hand : t -> string
end

module Make (C : Card) : Player with type card = C.t = struct
  type card = C.t
  type t = {
    id : int;
    hand : card list;
    chips : int;
  }

  let create id hand chips= {
    id;
    hand;
    chips;
  }

  let incr_chips player amount =
    { player with chips = player.chips + amount }

  let to_string player =
    Printf.sprintf "Player %d: %d chips" player.id player.chips

  let to_string_hand player =
    List.map C.to_string player.hand
    |> String.concat ", "
    |> Printf.sprintf "Player %d: %s" player.id
end