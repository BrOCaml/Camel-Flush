type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type t = suit * rank

(** AF: A card [t] represents a playing card with a [suit] and a [rank]. For
    example, (Spades, King) represents the King of Spades. *)

(** RI: A valid card [t] satisfies the following conditions:
    - [suit] is one of the four suits: Spades, Hearts, Diamonds, or Clubs.
    - [rank] is one of the 13 ranks: Ace, 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack,
      Queen, or King. *)

val compare : t -> t -> int
(** [compare a b] is the indication for order of a and b. If negative then a
    should be before b, if positive then b should before a. 0 if equal *)

val compare_rank : t -> t -> int
(** [compare_rank a b] is the indication for order of a and b based on rank. If
    negative then a should be before b, if positive then b should before a. 0 if
    equal *)

val to_string : t -> string
(** [to_string t] is the string representation of t *)

val to_code : t -> string
(** [to_code t] is the code representation of t *)

val rank_to_int : rank -> int
(**[rank_to_int r] is the integer representation of r*)

val rank_int_of_card : t -> int
(**[rank_int_of_card t] is the integer representation of the rank of t*)

val create_deck : t list
(**[create_deck] is the list of all cards in a deck*)

val suit : t -> suit
(**[suit t] is the suit of t*)

val rank : t -> rank
(**[rank t] is the rank of t*)
