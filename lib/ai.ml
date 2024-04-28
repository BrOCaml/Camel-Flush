open Unix

type t = PokerCard.t list

let create (lst : PokerCard.t list) = lst
let get_cards (t : t) = t
let add_card t card = t @ [ card ]
let card_lst_to_str_lst t = List.map PokerCard.to_code t
let str_lst_to_args lst = String.concat " " lst

let execute_prediction arg =
  try
    let command = Printf.sprintf "python3 lib/predict.py %s" arg in
    let ic, oc, ec = open_process_full command (environment ()) in
    let output = input_line ic in
    let _ = close_process_full (ic, oc, ec) in
    int_of_string output
  with _ ->
    let _ =
      print_endline "Error with AI\nDid you install all the dependencies?"
    in
    0

let predict t =
  t |> card_lst_to_str_lst |> str_lst_to_args |> execute_prediction
