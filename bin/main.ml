open Camel_flush

let () = print_endline "Hello, World!"
let round_number = 3

let rec prompt_and_print round_number =
  if round_number = 0 then print_string "Game Over!" else ()
