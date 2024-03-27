type card = PokerCard.t
type t = card list

let init = PokerCard.create_deck

let shuffle deck =
  let arr = Array.of_list deck in
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

let draw deck =
  match deck with
  | [] -> failwith "Deck is empty"
  | top :: rest -> (top, rest)

let to_string deck =
  match deck with
  | [] -> "[]"
  | deck -> "[" ^ String.concat "; " (List.map PokerCard.to_string deck) ^ "]"
