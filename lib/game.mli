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

val all_in : t -> player -> t
(** [all_in t p] is the new game status with player [p] bet all thier chips *)

val check : t -> player -> t
(** [check p] is the new game status with player [p] bet [0] chips *)

val call : t -> player -> t
(** [call p] is the new game status with player [p] bet the same amount as the
    previous player *)

val raise : t -> player -> int -> t
(** [raise p n] is the new game status with player [p] bet [n] more chips than
    the previous player *)

val action : t -> player -> string -> ?chips:int -> unit -> t
(** [action t p n] is the new game status with player [p] bet [n] chips *)

val bet_round : t -> t
(** [round t] is the new game status with the round advanced *)

val fold : t -> player -> t
(** [fold t p] is the new game status with player [p] folded *)

val to_string : t -> string
(** [to_string t] is the string representation of the game status *)

val player_best_combo : t -> string
(** [player_best_combo t] is the string representation of the player's best hand
    in the game *)
