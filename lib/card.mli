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
(**representation type of the card*)

val compare : t -> t -> int
(**[compare a b] is the indication for order of a and b. If negative then a
   should be before b, if positive then b should before a. 0 if equal*)

val to_string : t -> string
(**[to_string t] is the string representation of t*)

val rank_to_int : rank -> int
(**[rank_to_int r] is the integer representation of r*)
