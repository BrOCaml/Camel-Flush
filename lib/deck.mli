type card = PokerCard.t
type t

val init : t
(** [init ()] is a new deck of cards. *)

val shuffle : t -> t
(** [shuffle d] is the deck [d] with its cards shuffled. *)

val draw : t -> card * t
(** [draw d] is the top card of the deck [d] and the deck with the top card
    removed. *)

val to_string : t -> string
(** [to_string d] is a string representation of the deck [d]. *)
