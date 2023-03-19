exception Type_error
exception Name_error

val print_error : exn -> string -> Lexing.position -> unit
