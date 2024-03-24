module type Card = sig
  type t

  val to_string : t -> string
end

module type Deck = sig
  type card
  type t

  val init : t
  val shuffle : t -> t
  val draw : t -> card * t
end

module type Player = sig
  type card

  type t = {
    id : int;
    hand : card list;
    chips : int;
    is_fold : bool;
  }

  val is_fold : t -> bool
  val incr_chips : t -> int -> t
  val decr_chips : t -> int -> t
  val add_to_hand : t -> card -> t
end

module type Game = sig
  type deck
  (** The type of the game's deck *)

  type player
  (** The type of the game's players *)

  type card
  (** The type of the game's cards *)

  type t
  (** The type of the game status *)

  val init : int -> t
  (** [init] is the initial game status *)

  val deal : t -> t
  (** [deal t] is the new game status with each player dealt two cards *)

  val deal_community : t -> t
  (** [deal_community t] is the new game status with three community cards dealt *)

  val bet : t -> player -> int -> t
  (** [bet t p n] is the new game status with player [p] bet [n] chips *)

  val fold : t -> player -> t
  (** [fold t p] is the new game status with player [p] folded *)

  val to_string : t -> string
  (** [to_string t] is the string representation of the game status *)
end

module Make
    (C : Card)
    (D : Deck with type card = C.t)
    (P : Player with type card = C.t) :
  Game with type deck = D.t and type player = P.t and type card = C.t
