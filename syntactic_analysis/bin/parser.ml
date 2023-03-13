open Lexcal_analysis
module Lexer_token = Lexcal_analysis.Token
open Syntactic_analysis
module Parser_token = Syntactic_analysis.Token

(* If information on how far lexer/parser has analyzed is needed, remove this comment out.
   let tokenize_with_debug buf =
     let token = Parser_token.parsertoken_of_tokent (Lexer.read buf) in
     print_string (Parser_token.to_string token ^ " ");
     token
*)

let tokenize buf = Parser_token.parsertoken_of_tokent (Lexer.read buf)
let parse buf = Parser.main tokenize buf

let () =
  let fin = open_in Sys.argv.(1) in
  let buf = Lexing.from_channel fin in
  try parse buf with
  | Lexer.Parse_error e ->
      print_endline ("Error occurred in lexer: " ^ e);
      close_in fin
  | Lexer.Syntax_error e ->
      print_endline ("Error occurred in lexer: " ^ e);
      close_in fin
  | Parsing.Parse_error ->
      print_endline "Error occurred in parser: Parse error";
      close_in fin
