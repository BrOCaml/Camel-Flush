type card = PokerCard.t

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
val fold : t -> t
