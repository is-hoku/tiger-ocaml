open Lexical_analysis
module Lexer_token = Lexical_analysis.Token
open Semantic_action
module Parser_token = Semantic_action.Token

let tokenize buf = Parser_token.parsertoken_of_tokent (Lexer.read buf)
let parse buf = Parser.main tokenize buf
let print_ast ast = print_string (Syntax.string_of_exp ast)

let () =
  let fin = open_in "../../example_program/example3.tig" in
  let buf = Lexing.from_channel fin in
  try print_ast (parse buf) with
  | Lexer.Syntax_error e ->
      print_endline ("SyntaxError: " ^ e);
      close_in fin
  | Parsing.Parse_error ->
      print_endline "ParseError";
      close_in fin
