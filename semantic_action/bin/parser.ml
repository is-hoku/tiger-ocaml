open Lexical_analysis
module Lexer_token = Lexical_analysis.Token
open Semantic_action
module Parser_token = Semantic_action.Token

(* If information on how far lexer/parser has analyzed is needed, remove this comment out.
   let tokenize_with_debug buf =
     let token = Parser_token.parsertoken_of_tokent (Lexer.read buf) in
     print_string (Parser_token.to_string token ^ " ");
     token*)

let tokenize buf = Parser_token.parsertoken_of_tokent (Lexer.read buf)
let parse buf = Parser.main tokenize buf
let print_ast ast = print_string (Syntax.string_of_exp ast)

let () =
  let fin = open_in Sys.argv.(1) in
  let buf = Lexing.from_channel fin in
  try
    print_ast (parse buf);
    close_in fin
  with
  | Lexer.Syntax_error e ->
      print_endline ("SyntaxError: " ^ e);
      close_in fin
  | Parsing.Parse_error ->
      print_endline "SyntaxError";
      close_in fin
