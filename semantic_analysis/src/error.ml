exception Type_error
exception Name_error

let print_error error message (pos : Lexing.position) =
  match error with
  | Type_error ->
      let error_name = "TypeError" in
      Printf.fprintf stdout "%s: %s in line %d at character %d\n" error_name
        message pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol)
  | Name_error ->
      let error_name = "NameError" in
      Printf.fprintf stdout "%s: %s in line %d at character %d\n" error_name
        message pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol)
  | _ ->
      let error_name = "UnknownError" in
      Printf.fprintf stdout "%s: %s in line %d at character %d\n" error_name
        message pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol)
