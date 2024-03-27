type card = PokerCard.t

type t = {
  id : int;
  hand : card list;
  chips : int;
  is_fold : bool;
}

let is_fold player = player.is_fold
let create id hand chips = { id; hand; chips; is_fold = false }
let incr_chips player amount = { player with chips = player.chips + amount }
let decr_chips player amount = { player with chips = player.chips - amount }

let to_string player =
  Printf.sprintf "Player %d: %d chips" player.id player.chips

let to_string_hand player =
  List.map PokerCard.to_string player.hand
  |> String.concat ", "
  |> Printf.sprintf "Player %d: %s" player.id

let add_to_hand player card = { player with hand = card :: player.hand }
let fold player = { player with is_fold = true }
let all_in player = { player with chips = 0 }
