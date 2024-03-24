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

module Make (C : Card) : Player with type card = C.t
