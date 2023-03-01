%{
let parse_error s = print_endline s; flush stdout
%}

%token WHILE
%token FOR
%token TO
%token BREAK
%token LET
%token IN
%token END
%token FUNCTION
%token VAR
%token TYPE
%token ARRAY
%token IF
%token THEN
%token ELSE
%token DO
%token OF
%token NIL
%token ASSIGN
%token OR
%token AND
%token GE
%token GT
%token LE
%token LT
%token NEQ
%token EQ
%token DIVIDE
%token TIMES
%token MINUS
%token PLUS
%token DOT
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token SEMICOLON
%token COLON
%token COMMA
%token <int> INT
%token <string> ID
%token <string> STRING
%token EOF

%nonassoc OF
%nonassoc THEN
%nonassoc ELSE
%nonassoc DO
%nonassoc ASSIGN
%right LBRACK
%left OR
%left AND
%nonassoc EQ NEQ GT LT GE LE
%left PLUS MINUS
%left TIMES DIVIDE
%left UMINUS

%type <unit> main
%start main

/*%change EQ -> ASSIGN | ASSIGN -> EQ*/

%%

main:
| exp EOF {Printf.fprintf stdout "( %s eof )" $1}

decs:
| dec decs {Printf.sprintf "( %s %s )" $1 $2}
| {""}

dec:
| tydec {Printf.sprintf "( %s )" $1}
| vardec {Printf.sprintf "( %s )" $1}
| fundec {Printf.sprintf "( %s )" $1}

tydec:
| TYPE ID EQ ty {Printf.sprintf "( type id = %s )" $4}

ty:
| ID {"(id)"}
| LBRACE tyfields RBRACE {Printf.sprintf "( { %s } )" $2}
| ARRAY OF ID {"(array of id)"}

tyfields:
| ID COLON ID tyfield {Printf.sprintf "( id : id %s )" $4}
| {""}

tyfield:
| COMMA ID COLON ID tyfield {Printf.sprintf "( , id : id %s )" $5}
| {""}

vardec:
| VAR ID ASSIGN exp {Printf.sprintf "( var id := %s )" $4}
| VAR ID COLON ID ASSIGN exp {Printf.sprintf "( var id : id := %s )" $6}

fundec:
| FUNCTION ID LPAREN tyfields RPAREN EQ exp {Printf.sprintf "( function id (%s) = %s )" $4 $7}
| FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp {Printf.sprintf "( function id (%s) : id = %s )" $4 $9}

lvalue:
| ID {"( id )"} %prec LBRACK
| lvalue DOT ID {Printf.sprintf "( %s . id )" $1}
| lvalue LBRACK exp RBRACK {Printf.sprintf "( %s [ %s ] )" $1 $3}
| ID LBRACK exp RBRACK {Printf.sprintf "( id [ %s ] )" $3}

exp:
| lvalue {Printf.sprintf "( %s )" $1}
| NIL {"( nil )"}
| LPAREN seqs RPAREN {Printf.sprintf "( (%s) )" $2}
| INT {"( int )"}
| STRING {"( string )"}
| ID LPAREN params RPAREN {Printf.sprintf "( id (%s) )" $3}
| exp PLUS exp {Printf.sprintf "( %s + %s )" $1 $3}
| exp MINUS exp {Printf.sprintf "( %s - %s )" $1 $3}
| exp TIMES exp {Printf.sprintf "( %s * %s )" $1 $3}
| exp DIVIDE exp {Printf.sprintf "( %s / %s )" $1 $3}
| MINUS exp %prec UMINUS {Printf.sprintf "( - %s )" $2}
| exp EQ exp {Printf.sprintf "( %s = %s )" $1 $3}
| exp NEQ exp {Printf.sprintf "( %s <> %s )" $1 $3}
| exp GT exp {Printf.sprintf "( %s > %s )" $1 $3}
| exp LT exp {Printf.sprintf "( %s < %s )" $1 $3}
| exp GE exp {Printf.sprintf "( %s >= %s )" $1 $3}
| exp LE exp {Printf.sprintf "( %s <= %s )" $1 $3}
| exp AND exp {Printf.sprintf "( %s & %s )" $1 $3}
| exp OR exp {Printf.sprintf "( %s | %s )" $1 $3}
| ID LBRACE fields RBRACE {Printf.sprintf "( id { %s } )" $3}
| ID LBRACK exp RBRACK OF exp {Printf.sprintf "( id [ %s ] of %s )" $3 $6}
| lvalue ASSIGN exp {Printf.sprintf "( %s := %s )" $1 $3}
| IF exp THEN exp ELSE exp {Printf.sprintf "( if %s then %s else %s )" $2 $4 $6}
| IF exp THEN exp {Printf.sprintf "( if %s then %s )" $2 $4}
| WHILE exp DO exp {Printf.sprintf "( while %s do %s )" $2 $4}
| FOR ID ASSIGN exp TO exp DO exp {Printf.sprintf "( for id := %s to %s do %s )" $4 $6 $8}
| BREAK {"( break )"}
| LET decs IN expseqs END {Printf.sprintf "( let %s in %s end )" $2 $4}
| LPAREN exp RPAREN {Printf.sprintf "( (%s) )" $2}

expseqs:
| exp expseq {Printf.sprintf "( %s %s )" $1 $2}
| {""}

expseq:
| SEMICOLON exp expseq {Printf.sprintf "( ; %s %s )" $2 $3}
| {""}

seqs:
| exp SEMICOLON exp seq {Printf.sprintf "( %s ; %s %s )" $1 $3 $4}

seq:
| SEMICOLON exp seq {Printf.sprintf "( ; %s %s )" $2 $3}
| {""}

params:
| exp param {Printf.sprintf "( %s %s )" $1 $2}
| {""}

param:
| COMMA exp param {Printf.sprintf "( , %s %s )" $2 $3}
| {""}

fields:
| ID EQ exp field {Printf.sprintf "( id = %s %s )" $3 $4}
| {""}

field:
| COMMA ID EQ exp field {Printf.sprintf "( , id = %s %s )" $4 $5}
| {""}
