type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

let rank_to_int = function
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Ace -> 14

type t = suit * rank

let compare (s1, r1) (s2, r2) =
  match compare r1 r2 with
  | 0 -> compare s1 s2
  | n -> n

let compare_rank (_, r1) (_, r2) = Int.compare (rank_to_int r1) (rank_to_int r2)

let to_string (s, r) =
  let suit_str =
    match s with
    | Clubs -> "♣"
    | Diamonds -> "♦"
    | Hearts -> "♥"
    | Spades -> "♠"
  in
  let rank_str =
    match r with
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | Ace -> "A"
  in
  rank_str ^ suit_str

let suit (t : t) = fst t
let rank (t : t) = snd t

let create_deck =
  let suits = [ Clubs; Diamonds; Hearts; Spades ] in
  let ranks =
    [
      Two;
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
  List.map (fun s -> List.map (fun r -> (s, r)) ranks) suits |> List.flatten

let rank_int_of_card t = rank_to_int (rank t)
