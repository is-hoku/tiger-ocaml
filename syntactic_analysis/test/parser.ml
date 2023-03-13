open Lexical_analysis
module Lexer_token = Lexical_analysis.Token
open Syntactic_analysis
module Parser_token = Syntactic_analysis.Token

let tokenize buf = Parser_token.parsertoken_of_tokent (Lexer.read buf)
let parse buf = Parser.main tokenize buf

let () =
  let fin = open_in "../../example_program/test1.tig" in
  let buf = Lexing.from_channel fin in
  try parse buf with
  | Lexer.Syntax_error e ->
      print_endline ("SyntaxError: " ^ e);
      close_in fin
  | Parsing.Parse_error ->
      print_endline "Parse error";
      close_in fin
