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

val check : t -> player -> t
(** [check p] is the new game status with player [p] bet [0] chips *)

val fold : t -> player -> t
(** [fold t p] is the new game status with player [p] folded *)

val to_string : t -> string
(** [to_string t] is the string representation of the game status *)
