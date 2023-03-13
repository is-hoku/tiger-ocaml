open Lexcal_analysis

let rec tokenize buf =
  match Lexer.read buf with Token.EOF -> [] | t -> t :: tokenize buf

let print_token ch =
  ch |> Lexing.from_channel |> tokenize |> List.map Token.to_string
  |> String.concat " " |> print_string

let () =
  let fin = open_in Sys.argv.(1) in
  try print_token fin with
  | Lexer.Parse_error e ->
      print_endline ("Error occurred in lexer: " ^ e);
      close_in fin
  | Lexer.Syntax_error e ->
      print_endline ("Error occurred in lexer: " ^ e);
      close_in fin
