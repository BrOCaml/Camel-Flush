module type Card = sig
  type t

  val to_string : t -> string
end

module type Player = sig
  type card

  type t = {
    id : int;
    hand : card list;
    chips : int;
    is_fold : bool;
  }

  val create : int -> card list -> int -> t
  val incr_chips : t -> int -> t
  val decr_chips : t -> int -> t
  val to_string : t -> string
  val to_string_hand : t -> string
  val is_fold : t -> bool
  val add_to_hand : t -> card -> t
end

module Make (C : Card) : Player with type card = C.t = struct
  type card = C.t

  type t = {
    id : int;
    hand : card list;
    chips : int;
    is_fold : bool;
  }

  let is_fold player = player.is_fold
  let create id hand chips = { id; hand; chips; is_fold = false }
  let incr_chips player amount = { player with chips = player.chips + amount }
  let decr_chips player amount = { player with chips = player.chips - amount }

  let to_string player =
    Printf.sprintf "Player %d: %d chips" player.id player.chips

  let to_string_hand player =
    List.map C.to_string player.hand
    |> String.concat ", "
    |> Printf.sprintf "Player %d: %s" player.id

  let add_to_hand player card = { player with hand = card :: player.hand }
end
