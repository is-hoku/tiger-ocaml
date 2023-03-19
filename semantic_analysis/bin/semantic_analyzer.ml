open Lexical_analysis
module Lexer_token = Lexical_analysis.Token
open Semantic_action
module Parser_token = Semantic_action.Token
open Semantic_analysis

let tokenize buf = Parser_token.parsertoken_of_tokent (Lexer.read buf)
let parse buf = Parser.main tokenize buf

let semant venv tenv inside_loop exp =
  Semant.trans_exp (venv, tenv, inside_loop, exp)

let () =
  let fin = open_in Sys.argv.(1) in
  let buf = Lexing.from_channel fin in
  try
    let ast = parse buf in
    let expty = semant Env.base_venv Env.base_tenv false ast in
    print_endline (Types.string_of_ty expty.ty);
    close_in fin
  with
  | Lexer.Syntax_error e ->
      print_endline ("SyntaxError: " ^ e);
      close_in fin
  | Parsing.Parse_error ->
      print_endline "SyntaxError";
      close_in fin
