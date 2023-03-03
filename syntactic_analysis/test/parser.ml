open Lexcal_analysis
module Lexer_token = Lexcal_analysis.Token
open Syntactic_analysis
module Parser_token = Syntactic_analysis.Token

let tokenize buf = Parser_token.parsertoken_of_tokent (Lexer.read buf)
let parse buf = Parser.main tokenize buf

let () =
  let fin = open_in "../../example_program/test1.tig" in
  let buf = Lexing.from_channel fin in
  try parse buf with
  | Lexer.ParseError e -> print_endline ("Error occurred in lexer: " ^ e)
  | Lexer.SyntaxError e -> print_endline ("Error occurred in lexer: " ^ e)
  | Parsing.Parse_error -> print_endline "Error occurred in parser: Parse error"
