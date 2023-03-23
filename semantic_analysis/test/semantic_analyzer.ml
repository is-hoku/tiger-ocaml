open Lexical_analysis
module Lexer_token = Lexical_analysis.Token
open Semantic_action
module Parser_token = Semantic_action.Token
open Semantic_analysis

let tokenize buf = Parser_token.parsertoken_of_tokent (Lexer.read buf)
let parse buf = Parser.main tokenize buf

let semant venv tenv inside_loop exp =
  Semant.trans_exp (venv, tenv, inside_loop, exp)

let test_cases =
  [
    "merge.tig";
    "queens.tig";
    "recursive_type.tig";
    "test1.tig";
    "test10.tig";
    "test11.tig";
    "test12.tig";
    "test13.tig";
    "test14.tig";
    "test15.tig";
    "test16.tig";
    "test17.tig";
    "test18.tig";
    "test19.tig";
    "test2.tig";
    "test20.tig";
    "test21.tig";
    "test22.tig";
    "test23.tig";
    "test24.tig";
    "test25.tig";
    "test26.tig";
    "test27.tig";
    "test28.tig";
    "test29.tig";
    "test3.tig";
    "test30.tig";
    "test31.tig";
    "test32.tig";
    "test33.tig";
    "test34.tig";
    "test35.tig";
    "test36.tig";
    "test37.tig";
    "test38.tig";
    "test39.tig";
    "test4.tig";
    "test40.tig";
    "test41.tig";
    "test42.tig";
    "test43.tig";
    "test44.tig";
    "test45.tig";
    "test46.tig";
    "test47.tig";
    "test48.tig";
    "test49.tig";
    "test5.tig";
    "test6.tig";
    "test7.tig";
    "test8.tig";
    "test9.tig";
  ]

let () =
  List.iter
    (fun file_name ->
      let fin = open_in (Printf.sprintf "../../example_program/%s" file_name) in
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
      | Error.Type_error -> close_in fin
      | Error.Name_error -> close_in fin)
    test_cases
