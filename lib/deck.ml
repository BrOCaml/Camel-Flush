module type Card = sig
  type t

  val create_deck : t list
  val to_string : t -> string
end

module type Deck = sig
  type card
  type t

  val init : t
  val shuffle : t -> t
  val draw : t -> card * t
  val to_string : t -> string
  val generate_deck : unit -> (Card.suit * Card.rank) list
end

module Make (C : Card) : Deck with type card = C.t = struct
  type card = C.t
  type t = card list

  let init = C.create_deck

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
    | deck -> "[" ^ String.concat "; " (List.map C.to_string deck) ^ "]"

  let generate_suit_deck suit =
    let ranks =
      [
        Card.Two;
        Three;
        Four;
        Five;
        Six;
        Seven;
        Eight;
        Nine;
        Ten;
        Jack;
        Queen;
        King;
        Ace;
      ]
    in
    List.map (fun rank -> (suit, rank)) ranks

  let generate_deck () =
    let suits = [ Card.Clubs; Diamonds; Hearts; Spades ] in
    List.concat (List.map generate_suit_deck suits)
end
