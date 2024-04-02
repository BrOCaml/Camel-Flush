type t
(**Type representation for ai module*)

val create : PokerCard.t list -> t
(**[create cards] creates a new ai with the given cards*)

val get_cards : t -> PokerCard.t list
(**[get_cards ai] returns the cards of the ai*)

val add_card : t -> PokerCard.t -> t
(**[add_card ai card] adds the given cards to the ai*)

val predict : t -> int
(**[predict ai] returns the next move. 1 means fold. 0 means to check/raise*)

val execute_prediction : string -> int
(**[execute_prediction arg] is the result from executing the python file*)
