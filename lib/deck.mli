module type Card = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
end

module type Deck = sig
  type card
  type t

  val shuffle : t -> t
  (** [shuffle d] is the deck [d] with its cards shuffled. *)

  val draw : t -> card * t
  (** [draw d] is the top card of the deck [d] and the deck with the top card
      removed. *)

  val to_string : t -> string
  (** [to_string d] is a string representation of the deck [d]. *)

  val generate_deck : unit -> (Card.suit * Card.rank) list
  (** [generate_deck ()] is a complete and full deck. *)
end

module Make (C : Card) : Deck with type card = C.t
