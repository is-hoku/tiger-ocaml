open Lexical_analysis
module Lexer_token = Lexical_analysis.Token
open Semantic_action
module Parser_token = Semantic_action.Token
module Types = Semantic_analysis.Types
module Error = Semantic_analysis.Error
open Activation_record
module Env' = Activation_record.Env

let tokenize buf = Parser_token.parsertoken_of_tokent (Lexer.read buf)
let parse buf = Parser.main tokenize buf

let semant venv tenv inside_loop exp level =
  Semant.trans_exp (venv, tenv, inside_loop, exp, level)

let () =
  let fin = open_in "../../example_program/func1.tig" in
  let buf = Lexing.from_channel fin in
  try
    let ast = parse buf in
    Escape.find_escape ast;
    let level = Translate.outermost in
    let expty = semant Env'.base_venv Env'.base_tenv false ast level in
    print_endline (Translate.string_of_level level);
    print_endline (Types.string_of_ty expty.ty);
    close_in fin
  with
  | Lexer.Syntax_error e ->
      print_endline ("SyntaxError: " ^ e);
      close_in fin
  | Parsing.Parse_error ->
      print_endline "SyntaxError";
      close_in fin
  | Error.Type_error -> close_in fin
  | Error.Name_error -> close_in fin
