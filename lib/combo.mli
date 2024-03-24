type t
(**type representation of the combo*)

val new_combo : PokerCard.t list -> t
(**[new_combo] is the combo with list of 5 cards. Fail if list is not 5 cards in
   the list*)

val to_list : t -> PokerCard.t list
(**[to_list c] is the list of cards in the combo [c]*)

val to_string : t -> string
(**[to_string c] is the string representation of the combo [c]*)

val is_flush : t -> bool
(**[is_flush c] is true if the combo [c] is a flush, false otherwise*)

val is_straight : t -> bool
(**[is_straight c] is true if the combo [c] is a straight, false otherwise*)

val is_straight_flush : t -> bool
(**[is_straight_flush c] is true if the combo [c] is a straight flush, false
   otherwise*)

val is_royal_flush : t -> bool
(**[is_royal_flush c] is true if the combo [c] is a royal flush, false otherwise*)

val is_four_of_a_kind : t -> bool
(**[is_four_of_a_kind c] is true if the combo [c] is a four of a kind, false
   otherwise*)

val is_full_house : t -> bool
(**[is_full_house c] is true if the combo [c] is a full house, false otherwise*)

val is_three_of_a_kind : t -> bool
(**[is_three_of_a_kind c] is true if the combo [c] is a three of a kind, false
   otherwise*)

val is_two_pair : t -> bool
(**[is_two_pair c] is true if the combo [c] is a two pair, false otherwise*)

val is_one_pair : t -> bool
(**[is_pair c] is true if the combo [c] is a pair, false otherwise*)

val bro : int
