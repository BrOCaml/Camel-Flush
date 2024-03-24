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

module Make (C : Card) : Player with type card = C.t
