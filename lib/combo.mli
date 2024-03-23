type t
(**type representation of the combo*)

val new_combo : Card.t list -> t
(**[new_combo] is the combo with list of 5 cards. Fail if list is not 5 cards in
   the list*)

val to_list : t -> Card.t list
(**[to_list c] is the list of cards in the combo [c]*)

val to_string : t -> string
(**[to_string c] is the string representation of the combo [c]*)
