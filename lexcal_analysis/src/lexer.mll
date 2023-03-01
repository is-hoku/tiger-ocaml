{
open Lexing
open Token

exception ParseError of string
exception SyntaxError of string
}

let id = ['a'-'z' 'A'-'Z']+ ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int = ['0'-'9']+
let white = [' ' '\t']+

rule read = parse
| white { read lexbuf }
| '\n' { new_line lexbuf; read lexbuf }
| '"' { read_string (Buffer.create 16) lexbuf }
| "/*" { read_comment 1 lexbuf }
| "*/" { raise (SyntaxError (let pos = lexeme_start_p lexbuf in Printf.sprintf "invalid character %s in line %d at character %d" (lexeme lexbuf) pos.pos_lnum (pos.pos_cnum - pos.pos_bol))) }
| "while" { WHILE }
| "for" { FOR }
| "to" { TO }
| "break" { BREAK }
| "let" { LET }
| "in" { IN }
| "end" { END }
| "function" { FUNCTION }
| "var" { VAR }
| "type" { TYPE }
| "array" { ARRAY }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "do" { DO }
| "of" { OF }
| "nil" { NIL }
| ":=" { ASSIGN }
| "|" { OR }
| "&" { AND }
| ">=" { GE }
| ">" { GT }
| "<=" { LE }
| "<" { LT }
| "<>" { NEQ }
| "=" { EQ }
| "/" { DIVIDE }
| "*" { TIMES }
| "-" { MINUS }
| "+" { PLUS }
| "." { DOT }
| "{" { LBRACE }
| "}" { RBRACE }
| "[" { LBRACK }
| "]" { RBRACK }
| "(" { LPAREN }
| ")" { RPAREN }
| ";" { SEMICOLON }
| ":" { COLON }
| "," { COMMA }
| int { INT (int_of_string (lexeme lexbuf)) }
| id { ID (lexeme lexbuf) }
| _ { raise (ParseError (let pos = lexeme_start_p lexbuf in Printf.sprintf "unexpected character %s in line %d at character %d" (lexeme lexbuf) pos.pos_lnum (pos.pos_cnum - pos.pos_bol))) }
| eof { EOF }

and read_string buf = parse
| '"' { STRING (Buffer.contents buf) }
| '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
| '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
| '\\' '^' { read_caret buf lexbuf }
| '\\' '"' { Buffer.add_char buf '"'; read_string buf lexbuf }
| '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
| '\\' ('0' ['6'-'9'] ['0'-'9'] | '1' (['0'-'1'] ['0'-'9'] | '2' ['0'-'6'])) as dec { Buffer.add_char buf (Char.chr (int_of_string (String.sub dec 1 3))); read_string buf lexbuf }
| '\\' { read_ignored_string buf lexbuf }
| [^ '"' '\\']+ { Buffer.add_string buf (lexeme lexbuf); read_string buf lexbuf }
| _ { raise (ParseError (let pos = lexeme_start_p lexbuf in Printf.sprintf "invalid character %s in line %d at character %d" (lexeme lexbuf) pos.pos_lnum (pos.pos_cnum - pos.pos_bol))) }
| eof { raise (SyntaxError (let pos = lexeme_start_p lexbuf in Printf.sprintf "missing terminating \" character in line %d at character %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol))) }

and read_caret buf = parse
| '@' { Buffer.add_char buf '\000'; read_string buf lexbuf }
| 'G' { Buffer.add_char buf '\007'; read_string buf lexbuf }
| 'H' { Buffer.add_char buf '\008'; read_string buf lexbuf }
| 'I' { Buffer.add_char buf '\009'; read_string buf lexbuf }
| 'J' { Buffer.add_char buf '\010'; read_string buf lexbuf }
| 'K' { Buffer.add_char buf '\011'; read_string buf lexbuf }
| 'L' { Buffer.add_char buf '\012'; read_string buf lexbuf }
| 'M' { Buffer.add_char buf '\013'; read_string buf lexbuf }
| _ { raise (ParseError (let pos = lexeme_start_p lexbuf in Printf.sprintf "unexpected character as caret notation %s in line %d at character %d" (lexeme lexbuf) pos.pos_lnum (pos.pos_cnum - pos.pos_bol))) }

and read_ignored_string buf = parse
| '\\' { read_string buf lexbuf }
| ['\n' '\t' '\r' ' ']+ as s { Buffer.add_string buf s; read_ignored_string buf lexbuf }
| _ { raise (ParseError (let pos = lexeme_start_p lexbuf in Printf.sprintf "unexpected character in ignored characters %s in line %d at character %d" (lexeme lexbuf) pos.pos_lnum (pos.pos_cnum - pos.pos_bol))) }

and read_comment opened = parse
| "/*" { read_comment (opened+1) lexbuf }
| "*/" { match opened-1 with
        | 0 -> read lexbuf
        | _ -> read_comment (opened-1) lexbuf
       }
| _ { read_comment opened lexbuf }
| eof { raise (SyntaxError (let pos = lexeme_start_p lexbuf in Printf.sprintf "missing terminating */ character in line %d at character %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol))) }
