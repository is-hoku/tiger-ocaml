type t =
  | WHILE
  | FOR
  | TO
  | BREAK
  | LET
  | IN
  | END
  | FUNCTION
  | VAR
  | TYPE
  | ARRAY
  | IF
  | THEN
  | ELSE
  | DO
  | OF
  | NIL
  | ASSIGN
  | OR
  | AND
  | GE
  | GT
  | LE
  | LT
  | NEQ
  | EQ
  | DIVIDE
  | TIMES
  | MINUS
  | PLUS
  | DOT
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | SEMICOLON
  | COLON
  | COMMA
  | INT of int
  | ID of string
  | STRING of string
  | EOF

let to_string t =
  match t with
  | WHILE -> "while"
  | FOR -> "for"
  | TO -> "to"
  | BREAK -> "break"
  | LET -> "let"
  | IN -> "in"
  | END -> "end"
  | FUNCTION -> "function"
  | VAR -> "var"
  | TYPE -> "type"
  | ARRAY -> "array"
  | IF -> "if"
  | THEN -> "then"
  | ELSE -> "else"
  | DO -> "do"
  | OF -> "of"
  | NIL -> "nil"
  | ASSIGN -> ":="
  | OR -> "|"
  | AND -> "&"
  | GE -> ">="
  | GT -> ">"
  | LE -> "<="
  | LT -> "<"
  | NEQ -> "<>"
  | EQ -> "="
  | DIVIDE -> "/"
  | TIMES -> "*"
  | MINUS -> "-"
  | PLUS -> "+"
  | DOT -> "."
  | LBRACE -> "{"
  | RBRACE -> "}"
  | LBRACK -> "["
  | RBRACK -> "]"
  | LPAREN -> "("
  | RPAREN -> ")"
  | SEMICOLON -> ";"
  | COLON -> ":"
  | COMMA -> ","
  | INT n -> string_of_int n
  | ID s -> s
  | STRING s -> s
  | EOF -> ""
