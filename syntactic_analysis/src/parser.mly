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

%%

main:
| exp EOF { Printf.fprintf stdout "( %s eof )" $1 }
| error { let pos = Parsing.symbol_end_pos () in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) }
decs:
| dec decs { Printf.sprintf "( %s %s )" $1 $2 }
| { "" }

dec:
| tydec { Printf.sprintf "( %s )" $1 }
| vardec { Printf.sprintf "( %s )" $1 }
| fundec { Printf.sprintf "( %s )" $1 }

tydec:
| TYPE ID EQ ty { Printf.sprintf "( type id = %s )" $4 }
/* error */
| TYPE error EQ ty { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( type error = %s )" $4 }
| TYPE ID error ty { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( type id error %s )" $4 }


ty:
| ID { "(id)" }
| LBRACE tyfields RBRACE { Printf.sprintf "( { %s } )" $2 }
| ARRAY OF ID { "( array of id )" }
/* error */
| ARRAY error ID { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); "( array error id )" }

tyfields:
| ID COLON ID tyfield { Printf.sprintf "( id : id %s )" $4 }
| { "" }
/* error */
| ID error ID tyfield { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( id error id %s )" $4 }
| ID COLON error tyfield { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( id : error %s )" $4 }

tyfield:
| COMMA ID COLON ID tyfield { Printf.sprintf "( , id : id %s )" $5 }
| { "" }
/* error */
| COMMA error COLON ID tyfield { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( , error : id %s )" $5 }
| COMMA ID error ID tyfield { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( , id error id %s )" $5 }
| COMMA ID COLON error tyfield { let pos = Parsing.rhs_end_pos 4 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( , id : error %s )" $5 }

vardec:
| VAR ID ASSIGN exp { Printf.sprintf "( var id := %s )" $4 }
| VAR ID COLON ID ASSIGN exp { Printf.sprintf "( var id : id := %s )" $6 }
/* error */
| VAR error ASSIGN exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( var error := %s )" $4 }
| VAR ID error exp { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( var id error %s )" $4 }
| VAR error COLON ID ASSIGN exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( var error : id := %s )" $6 }
| VAR ID error ID ASSIGN exp { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( var id error id := %s )" $6 }
| VAR ID COLON error ASSIGN exp { let pos = Parsing.rhs_end_pos 4 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( var id : error := %s )" $6 }
| VAR ID COLON ID error exp { let pos = Parsing.rhs_end_pos 5 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( var id : id error %s )" $6 }

fundec:
| FUNCTION ID LPAREN tyfields RPAREN EQ exp { Printf.sprintf "( function id (%s) = %s )" $4 $7 }
| FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp { Printf.sprintf "( function id (%s) : id = %s )" $4 $9 }
/* error */
| FUNCTION error LPAREN tyfields RPAREN EQ exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( function error (%s) = %s )" $4 $7 }
| FUNCTION ID error tyfields RPAREN EQ exp { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( function id error %s) = %s )" $4 $7 }
| FUNCTION ID LPAREN tyfields error EQ exp { let pos = Parsing.rhs_end_pos 5 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( function id (%s error = %s )" $4 $7 }
| FUNCTION ID LPAREN tyfields RPAREN error exp { let pos = Parsing.rhs_end_pos 6 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( function id (%s) error %s )" $4 $7 }
| FUNCTION error LPAREN tyfields RPAREN COLON ID EQ exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( function error (%s) : id = %s )" $4 $9 }
| FUNCTION ID error tyfields RPAREN COLON ID EQ exp { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( function id error %s) : id = %s )" $4 $9 }
| FUNCTION ID LPAREN tyfields error COLON ID EQ exp { let pos = Parsing.rhs_end_pos 5 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( function id (%s error : id = %s )" $4 $9 }
| FUNCTION ID LPAREN tyfields RPAREN error ID EQ exp { let pos = Parsing.rhs_end_pos 6 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( function id (%s) error id = %s )" $4 $9 }
| FUNCTION ID LPAREN tyfields RPAREN COLON error EQ exp { let pos = Parsing.rhs_end_pos 7 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( function id (%s) : error = %s )" $4 $9 }
| FUNCTION ID LPAREN tyfields RPAREN COLON ID error exp { let pos = Parsing.rhs_end_pos 8 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( function id (%s) : id error %s )" $4 $9 }

lvalue:
| ID { "( id )" } %prec LBRACK
| lvalue DOT ID { Printf.sprintf "( %s . id )" $1 }
| lvalue LBRACK exp RBRACK { Printf.sprintf "( %s [ %s ] )" $1 $3 }
| ID LBRACK exp RBRACK { Printf.sprintf "( id [ %s ] )" $3 }
/* error */
| lvalue LBRACK error RBRACK { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( %s (error) )" $1 }
| ID LBRACK error RBRACK { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); "( id [ error ] )" }

exp:
| lvalue { Printf.sprintf "( %s )" $1 }
| NIL { "( nil )" }
| LPAREN seqs RPAREN { Printf.sprintf "( (%s) )" $2 }
| INT { "( int )" }
| STRING { "( string )" }
| ID LPAREN params RPAREN { Printf.sprintf "( id (%s) )" $3 }
| exp PLUS exp { Printf.sprintf "( %s + %s )" $1 $3 }
| exp MINUS exp { Printf.sprintf "( %s - %s )" $1 $3 }
| exp TIMES exp { Printf.sprintf "( %s * %s )" $1 $3 }
| exp DIVIDE exp { Printf.sprintf "( %s / %s )" $1 $3 }
| MINUS exp %prec UMINUS { Printf.sprintf "( - %s )" $2 }
| exp EQ exp { Printf.sprintf "( %s = %s )" $1 $3 }
| exp NEQ exp { Printf.sprintf "( %s <> %s )" $1 $3 }
| exp GT exp { Printf.sprintf "( %s > %s )" $1 $3 }
| exp LT exp { Printf.sprintf "( %s < %s )" $1 $3 }
| exp GE exp { Printf.sprintf "( %s >= %s )" $1 $3 }
| exp LE exp { Printf.sprintf "( %s <= %s )" $1 $3 }
| exp AND exp { Printf.sprintf "( %s & %s )" $1 $3 }
| exp OR exp { Printf.sprintf "( %s | %s )" $1 $3 }
| ID LBRACE fields RBRACE { Printf.sprintf "( id { %s } )" $3 }
| ID LBRACK exp RBRACK OF exp { Printf.sprintf "( id [ %s ] of %s )" $3 $6 }
| lvalue ASSIGN exp { Printf.sprintf "( %s := %s )" $1 $3 }
| IF exp THEN exp ELSE exp { Printf.sprintf "( if %s then %s else %s )" $2 $4 $6 }
| IF exp THEN exp { Printf.sprintf "( if %s then %s )" $2 $4 }
| WHILE exp DO exp { Printf.sprintf "( while %s do %s )" $2 $4 }
| FOR ID ASSIGN exp TO exp DO exp { Printf.sprintf "( for id := %s to %s do %s )" $4 $6 $8 }
| BREAK { "( break )" }
| LET decs IN expseqs END { Printf.sprintf "( let %s in %s end )" $2 $4 }
| LPAREN exp RPAREN { Printf.sprintf "( (%s) )" $2 }
/* error */
| LPAREN error RPAREN { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); "( (error) )" }
| ID LBRACE error RBRACE { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); "( id { error } )" }
| ID LBRACK error RBRACK OF exp { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( id [ error ] of %s )" $6 }
| ID LBRACK exp error OF exp { let pos = Parsing.rhs_end_pos 4 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( id [ %s error of %s )" $3 $6 }
| IF error THEN exp ELSE exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( if error then %s else %s )" $4 $6 }
| IF exp error exp ELSE exp { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( if %s error %s else %s )" $2 $4 $6 }
| IF exp THEN error ELSE exp { let pos = Parsing.rhs_end_pos 4 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( if %s then error else %s )" $2 $6 }
| IF error THEN exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( if error then %s )" $4 }
| WHILE error DO exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( while error do %s )" $4 }
| FOR error ASSIGN exp TO exp DO exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( for error := %s to %s do %s )" $4 $6 $8 }
| FOR ID error exp TO exp DO exp { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( for id error %s to %s do %s )" $4 $6 $8 }
| FOR ID ASSIGN error TO exp DO exp { let pos = Parsing.rhs_end_pos 4 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( for id := error to %s do %s )" $6 $8 }
| FOR ID ASSIGN exp error exp DO exp { let pos = Parsing.rhs_end_pos 5 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( for id := %s error %s do %s )" $4 $6 $8 }
| FOR ID ASSIGN exp TO error DO exp { let pos = Parsing.rhs_end_pos 6 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( for id := %s to error do %s )" $4 $8 }
| LET error IN expseqs END { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( let error in %s end )" $4 }
| LET decs IN error END { let pos = Parsing.rhs_end_pos 4 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( let %s in error end )" $2 }

expseqs:
| exp expseq { Printf.sprintf "( %s %s )" $1 $2 }
| { "" }

expseq:
| SEMICOLON exp expseq { Printf.sprintf "( ; %s %s )" $2 $3 }
| { "" }
/* error */
| SEMICOLON error expseq { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( ; error %s )" $3 }

seqs:
| exp SEMICOLON exp seq { Printf.sprintf "( %s ; %s %s )" $1 $3 $4 }
/* error */
| exp SEMICOLON error seq { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( %s ; error %s )" $1 $4 }

seq:
| SEMICOLON exp seq { Printf.sprintf "( ; %s %s )" $2 $3 }
| { "" }
/* error */
| SEMICOLON error seq { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( ; error %s )" $3 }

params:
| exp param { Printf.sprintf "( %s %s )" $1 $2 }
| { "" }

param:
| COMMA exp param { Printf.sprintf "( , %s %s )" $2 $3 }
| { "" }
/* error */
| COMMA error param { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( , error %s )" $3 }

fields:
| ID EQ exp field { Printf.sprintf "( id = %s %s )" $3 $4 }
| { "" }
/* error */
| ID error exp field { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( id error %s %s )" $3 $4 }
| ID EQ error field { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( id = error %s )" $4 }

field:
| COMMA ID EQ exp field { Printf.sprintf "( , id = %s %s )" $4 $5 }
| { "" }
/* error */
| COMMA error EQ exp field { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( , error = %s %s )" $4 $5 }
| COMMA ID EQ error field { let pos = Parsing.rhs_end_pos 4 in Printf.fprintf stdout "invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Printf.sprintf "( , id = error %s )" $5 }
