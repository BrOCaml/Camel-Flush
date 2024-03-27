type card = PokerCard.t
(** representation type of card *)

type t = {
  id : int;
  hand : card list;
  chips : int;
  is_fold : bool;
}
(** represents a player in the game *)

val create : int -> card list -> int -> t
(** [create id hand chips] creates a new player with specifcied name [id], an
    initial [hand], and initial bet amouunt [chips] *)

val incr_chips : t -> int -> t
(** [incr_chips player amount] increases the [player] chips by [amount] *)

val decr_chips : t -> int -> t
(** [decr_chips player amount] decreases the [player] chips by [amount] *)

val to_string : t -> string
(** [to_string player] is a string representation of [player] with [chips] *)

val to_string_hand : t -> string
(** [to_string_hand player] is a string representation of [player] hand *)

val is_fold : t -> bool
(** [is_fold player] is true if [player] folded, and false otherwise *)

val add_to_hand : t -> card -> t
(** [add_to_hand player card] adds [card] to [player] hand *)

val fold : t -> t
(** [fold player] folds the [player] hand *)

val all_in : t -> t
(** [all_in player] sets the [player] bet to his total current chips *)
