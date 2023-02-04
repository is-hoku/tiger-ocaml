open Lexcal_analysis

let rec tokenize buf =
  match Lexer.read buf with
  | Token.EOF -> [ Token.EOF ]
  | t -> t :: tokenize buf

let print_token ch =
  ch |> Lexing.from_channel |> tokenize |> List.map Token.to_string
  |> List.map (fun x -> x ^ " ")
  |> List.iter print_string

let () =
  let fin = open_in Sys.argv.(1) in
  print_token fin
