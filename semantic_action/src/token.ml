open Lexical_analysis.Token

let parsertoken_of_tokent = function
  | WHILE -> Parser.WHILE
  | FOR -> Parser.FOR
  | TO -> Parser.TO
  | BREAK -> Parser.BREAK
  | LET -> Parser.LET
  | IN -> Parser.IN
  | END -> Parser.END
  | FUNCTION -> Parser.FUNCTION
  | VAR -> Parser.VAR
  | TYPE -> Parser.TYPE
  | ARRAY -> Parser.ARRAY
  | IF -> Parser.IF
  | THEN -> Parser.THEN
  | ELSE -> Parser.ELSE
  | DO -> Parser.DO
  | OF -> Parser.OF
  | NIL -> Parser.NIL
  | ASSIGN -> Parser.ASSIGN
  | OR -> Parser.OR
  | AND -> Parser.AND
  | GE -> Parser.GE
  | GT -> Parser.GT
  | LE -> Parser.LE
  | LT -> Parser.LT
  | NEQ -> Parser.NEQ
  | EQ -> Parser.EQ
  | DIVIDE -> Parser.DIVIDE
  | TIMES -> Parser.TIMES
  | MINUS -> Parser.MINUS
  | PLUS -> Parser.PLUS
  | DOT -> Parser.DOT
  | LBRACE -> Parser.LBRACE
  | RBRACE -> Parser.RBRACE
  | LBRACK -> Parser.LBRACK
  | RBRACK -> Parser.RBRACK
  | LPAREN -> Parser.LPAREN
  | RPAREN -> Parser.RPAREN
  | SEMICOLON -> Parser.SEMICOLON
  | COLON -> Parser.COLON
  | COMMA -> Parser.COMMA
  | INT n -> Parser.INT n
  | ID s -> Parser.ID s
  | STRING s -> Parser.STRING s
  | EOF -> Parser.EOF

let to_string = function
  | Parser.WHILE -> "while"
  | Parser.FOR -> "for"
  | Parser.TO -> "to"
  | Parser.BREAK -> "break"
  | Parser.LET -> "let"
  | Parser.IN -> "in"
  | Parser.END -> "end"
  | Parser.FUNCTION -> "function"
  | Parser.VAR -> "var"
  | Parser.TYPE -> "type"
  | Parser.ARRAY -> "array"
  | Parser.IF -> "if"
  | Parser.THEN -> "then"
  | Parser.ELSE -> "else"
  | Parser.DO -> "do"
  | Parser.OF -> "of"
  | Parser.NIL -> "nil"
  | Parser.ASSIGN -> ":="
  | Parser.OR -> "|"
  | Parser.AND -> "&"
  | Parser.GE -> ">="
  | Parser.GT -> ">"
  | Parser.LE -> "<="
  | Parser.LT -> "<"
  | Parser.NEQ -> "<>"
  | Parser.EQ -> "="
  | Parser.DIVIDE -> "/"
  | Parser.TIMES -> "*"
  | Parser.MINUS -> "-"
  | Parser.PLUS -> "+"
  | Parser.DOT -> "."
  | Parser.LBRACE -> "{"
  | Parser.RBRACE -> "}"
  | Parser.LBRACK -> "["
  | Parser.RBRACK -> "]"
  | Parser.LPAREN -> "("
  | Parser.RPAREN -> ")"
  | Parser.SEMICOLON -> ";"
  | Parser.COLON -> ":"
  | Parser.COMMA -> ","
  | Parser.INT n -> string_of_int n
  | Parser.ID s -> "id(" ^ s ^ ")"
  | Parser.STRING s -> s
  | Parser.EOF -> ""
