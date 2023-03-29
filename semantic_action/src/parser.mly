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

%type <Syntax.exp> main
%start main

%%

main:
| exp EOF { $1 }
| error { let pos = Parsing.symbol_end_pos () in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); ErrorExp}

decs:
| dec decs { $1 @ $2 }
| { [] }

dec:
| tydec tydecs { [Syntax.TypeDec ($1 @ $2)] }
| vardec { [$1] }
| fundec fundecs { [Syntax.FunctionDec ($1 @ $2)] }

tydecs:
| tydec tydecs { $1 @ $2 }
| { [] }

fundecs:
| fundec fundecs { $1 @ $2 }
| { [] }

tydec:
| TYPE ID EQ ty { [{tname = Syntax.Sym (Symbol.from_string $2); ty = $4; tpos = Parsing.rhs_end_pos 1}] }
/* error */
| TYPE error EQ ty { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); [{tname = Syntax.ErrorSym; ty = $4; tpos = Parsing.rhs_end_pos 1}] }
| TYPE ID error ty { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); [{tname = Syntax.Sym (Symbol.from_string $2); ty = $4; tpos = Parsing.rhs_end_pos 1}] }


ty:
| ID { Syntax.NameTy ((Syntax.Sym (Symbol.from_string $1)), (Parsing.rhs_end_pos 1)) }
| LBRACE tyfields RBRACE { Syntax.RecordTy $2 }
| ARRAY OF ID { Syntax.ArrayTy ((Syntax.Sym (Symbol.from_string $3)), (Parsing.rhs_end_pos 1)) }
/* error */
| ARRAY error ID { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.ArrayTy ( Syntax.Sym (Symbol.from_string $3), (Parsing.rhs_end_pos 1)) }

tyfields:
| ID COLON ID tyfield { {name = Syntax.Sym (Symbol.from_string $1); escape = ref true; typ = Syntax.Sym (Symbol.from_string $3); pos = Parsing.rhs_end_pos 1} :: $4 }
| { [] }
/* error */
| ID error ID tyfield { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); {name = Syntax.Sym (Symbol.from_string $1); escape = ref true; typ = Syntax.Sym (Symbol.from_string $3); pos = Parsing.rhs_end_pos 1} :: $4 }
| ID COLON error tyfield { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); {name = Syntax.Sym (Symbol.from_string $1); escape = ref true; typ = Syntax.ErrorSym; pos = Parsing.rhs_end_pos 1} :: $4 }

tyfield:
| COMMA ID COLON ID tyfield { {name = Syntax.Sym (Symbol.from_string $2); escape = ref true; typ = Syntax.Sym (Symbol.from_string $4); pos = Parsing.rhs_end_pos 2} :: $5 }
| { [] }
/* error */
| COMMA error COLON ID tyfield { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); {name = Syntax.ErrorSym; escape = ref true; typ = Syntax.Sym (Symbol.from_string $4); pos = Parsing.rhs_end_pos 2} :: $5 }
| COMMA ID error ID tyfield { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); {name = Syntax.Sym (Symbol.from_string $2); escape = ref true; typ = Syntax.Sym (Symbol.from_string $4); pos = Parsing.rhs_end_pos 2} :: $5 }
| COMMA ID COLON error tyfield { let pos = Parsing.rhs_end_pos 4 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); {name = Syntax.Sym (Symbol.from_string $2); escape = ref true; typ = Syntax.ErrorSym; pos = Parsing.rhs_end_pos 2} :: $5 }

vardec:
| VAR ID ASSIGN exp { Syntax.VarDec {name = Syntax.Sym (Symbol.from_string $2); escape = ref true; typ = None; init = $4; pos = Parsing.rhs_end_pos 1} }
| VAR ID COLON ID ASSIGN exp { Syntax.VarDec {name = Syntax.Sym (Symbol.from_string $2); escape = ref true; typ = Some ((Syntax.Sym (Symbol.from_string $4)), (Parsing.rhs_end_pos 4)); init = $6; pos = Parsing.rhs_end_pos 1} }
/* error */
| VAR error ASSIGN exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.VarDec {name = Syntax.ErrorSym; escape = ref true; typ = None; init = $4; pos = Parsing.rhs_end_pos 1} }
| VAR ID error exp { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.VarDec {name = Syntax.Sym (Symbol.from_string $2); escape = ref true; typ = None; init = $4; pos = Parsing.rhs_end_pos 1}}
| VAR error COLON ID ASSIGN exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.VarDec {name = Syntax.ErrorSym; escape = ref true; typ = Some ((Syntax.Sym (Symbol.from_string $4)), (Parsing.rhs_end_pos 4)); init = $6; pos = Parsing.rhs_end_pos 1} }
| VAR ID error ID ASSIGN exp { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.VarDec {name = Syntax.Sym (Symbol.from_string $2); escape = ref true; typ = Some ((Syntax.Sym (Symbol.from_string $4)), (Parsing.rhs_end_pos 4)); init = $6; pos = Parsing.rhs_end_pos 1} }
| VAR ID COLON error ASSIGN exp { let pos = Parsing.rhs_end_pos 4 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.VarDec {name = Syntax.Sym (Symbol.from_string $2); escape = ref true; typ = Some (Syntax.ErrorSym, (Parsing.rhs_end_pos 4)); init = $6; pos = Parsing.rhs_end_pos 1} }
| VAR ID COLON ID error exp { let pos = Parsing.rhs_end_pos 5 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.VarDec {name = Syntax.Sym (Symbol.from_string $2); escape = ref true; typ = Some ((Syntax.Sym (Symbol.from_string $4)), (Parsing.rhs_end_pos 4)); init = $6; pos = Parsing.rhs_end_pos 1} }

fundec:
| FUNCTION ID LPAREN tyfields RPAREN EQ exp { [{fname = Syntax.Sym (Symbol.from_string $2); params = $4; result = None; body = $7; fpos = Parsing.rhs_end_pos 1}] }
| FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp { [{fname = Syntax.Sym (Symbol.from_string $2); params = $4; result = Some ((Syntax.Sym (Symbol.from_string $7)), (Parsing.rhs_end_pos 7)); body = $9; fpos = Parsing.rhs_end_pos 1}] }
/* error */
| FUNCTION error LPAREN tyfields RPAREN EQ exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); [{fname = Syntax.ErrorSym; params = $4; result = None; body = $7; fpos = Parsing.rhs_end_pos 1}] }
| FUNCTION ID error tyfields RPAREN EQ exp { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); [{fname = Syntax.Sym (Symbol.from_string $2); params = $4; result = None; body = $7; fpos = Parsing.rhs_end_pos 1}] }
| FUNCTION ID LPAREN tyfields error EQ exp { let pos = Parsing.rhs_end_pos 5 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); [{fname = Syntax.Sym (Symbol.from_string $2); params = $4; result = None; body = $7; fpos = Parsing.rhs_end_pos 1}] }
| FUNCTION ID LPAREN tyfields RPAREN error exp { let pos = Parsing.rhs_end_pos 6 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); [{fname = Syntax.Sym (Symbol.from_string $2); params = $4; result = None; body = $7; fpos = Parsing.rhs_end_pos 1}] }
| FUNCTION error LPAREN tyfields RPAREN COLON ID EQ exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); [{fname = Syntax.ErrorSym; params = $4; result = Some ((Syntax.Sym (Symbol.from_string $7)), (Parsing.rhs_end_pos 7)); body = $9; fpos = Parsing.rhs_end_pos 1}] }
| FUNCTION ID error tyfields RPAREN COLON ID EQ exp { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); [{fname = Syntax.Sym (Symbol.from_string $2); params = $4; result = Some ((Syntax.Sym (Symbol.from_string $7)), (Parsing.rhs_end_pos 7)); body = $9; fpos = Parsing.rhs_end_pos 1}] }
| FUNCTION ID LPAREN tyfields error COLON ID EQ exp { let pos = Parsing.rhs_end_pos 5 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); [{fname = Syntax.Sym (Symbol.from_string $2); params = $4; result = Some ((Syntax.Sym (Symbol.from_string $7)), (Parsing.rhs_end_pos 7)); body = $9; fpos = Parsing.rhs_end_pos 1}] }
| FUNCTION ID LPAREN tyfields RPAREN error ID EQ exp { let pos = Parsing.rhs_end_pos 6 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); [{fname = Syntax.Sym (Symbol.from_string $2); params = $4; result = Some ((Syntax.Sym (Symbol.from_string $7)), (Parsing.rhs_end_pos 7)); body = $9; fpos = Parsing.rhs_end_pos 1}] }
| FUNCTION ID LPAREN tyfields RPAREN COLON error EQ exp { let pos = Parsing.rhs_end_pos 7 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); [{fname = Syntax.Sym (Symbol.from_string $2); params = $4; result = Some ((Syntax.ErrorSym), (Parsing.rhs_end_pos 7)); body = $9; fpos = Parsing.rhs_end_pos 1}] }
| FUNCTION ID LPAREN tyfields RPAREN COLON ID error exp { let pos = Parsing.rhs_end_pos 8 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); [{fname = Syntax.Sym (Symbol.from_string $2); params = $4; result = Some ((Syntax.Sym (Symbol.from_string $7)), (Parsing.rhs_end_pos 7)); body = $9; fpos = Parsing.rhs_end_pos 1}] }

lvalue:
| ID { Syntax.SimpleVar (Syntax.Sym (Symbol.from_string $1), Parsing.rhs_end_pos 1) } %prec LBRACK
| lvalue DOT ID { Syntax.FieldVar ($1, Syntax.Sym (Symbol.from_string $3), Parsing.rhs_end_pos 1) }
| lvalue LBRACK exp RBRACK { Syntax.SubscriptVar ($1, $3, Parsing.rhs_end_pos 1) }
| ID LBRACK exp RBRACK { Syntax.SubscriptVar (Syntax.SimpleVar (Syntax.Sym (Symbol.from_string $1), Parsing.rhs_end_pos 1), $3, Parsing.rhs_end_pos 1) }
/* error */
| lvalue LBRACK error RBRACK { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.SubscriptVar ($1, Syntax.ErrorExp, Parsing.rhs_end_pos 1) }
| ID LBRACK error RBRACK { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.SubscriptVar (Syntax.SimpleVar (Syntax.Sym (Symbol.from_string $1), Parsing.rhs_end_pos 1), Syntax.ErrorExp, Parsing.rhs_end_pos 1) }

exp:
| lvalue { Syntax.VarExp $1 }
| NIL { Syntax.NilExp }
| LPAREN seqs RPAREN { $2 }
| INT { Syntax.IntExp $1 }
| STRING { Syntax.StringExp ($1, Parsing.rhs_end_pos 1) }
| LPAREN RPAREN { Syntax.SeqExp [] }
| ID LPAREN params RPAREN { Syntax.CallExp {func = Syntax.Sym (Symbol.from_string $1); args = $3; pos = Parsing.rhs_end_pos 1} }
| exp PLUS exp { Syntax.OpExp {left = $1; oper = Syntax.PlusOp; right = $3; pos = Parsing.rhs_end_pos 2} }
| exp MINUS exp { Syntax.OpExp {left = $1; oper = Syntax.MinusOp; right = $3; pos = Parsing.rhs_end_pos 2} }
| exp TIMES exp { Syntax.OpExp {left = $1; oper = Syntax.TimesOp; right = $3; pos = Parsing.rhs_end_pos 2} }
| exp DIVIDE exp { Syntax.OpExp {left = $1; oper = Syntax.DivideOp; right = $3; pos = Parsing.rhs_end_pos 2} }
| MINUS exp %prec UMINUS { Syntax.OpExp {left = Syntax.IntExp 0; oper = Syntax.MinusOp; right = $2; pos = Parsing.rhs_end_pos 1} }
| exp EQ exp { Syntax.OpExp {left = $1; oper = Syntax.EqOp; right = $3; pos = Parsing.rhs_end_pos 2} }
| exp NEQ exp { Syntax.OpExp {left = $1; oper = Syntax.NeqOp; right = $3; pos = Parsing.rhs_end_pos 2} }
| exp GT exp { Syntax.OpExp {left = $1; oper = Syntax.GtOp; right = $3; pos = Parsing.rhs_end_pos 2} }
| exp LT exp { Syntax.OpExp {left = $1; oper = Syntax.LtOp; right = $3; pos = Parsing.rhs_end_pos 2} }
| exp GE exp { Syntax.OpExp {left = $1; oper = Syntax.GeOp; right = $3; pos = Parsing.rhs_end_pos 2} }
| exp LE exp { Syntax.OpExp {left = $1; oper = Syntax.LeOp; right = $3; pos = Parsing.rhs_end_pos 2} }
| exp AND exp { Syntax.IfExp {test = $1; then' = $3; else' = Some (Syntax.IntExp 0); pos = Parsing.rhs_end_pos 2} }
| exp OR exp { Syntax.IfExp {test = $1; then' = Syntax.IntExp 1; else' = Some $3; pos = Parsing.rhs_end_pos 2} }
| ID LBRACE fields RBRACE { Syntax.RecordExp {fields = $3; typ = Syntax.Sym (Symbol.from_string $1); pos = Parsing.rhs_end_pos 1} }
| ID LBRACK exp RBRACK OF exp { Syntax.ArrayExp {typ = Syntax.Sym (Symbol.from_string $1); size = $3; init = $6; pos = Parsing.rhs_end_pos 1} }
| lvalue ASSIGN exp { Syntax.AssignExp {var = $1; exp = $3; pos = Parsing.rhs_end_pos 2} }
| IF exp THEN exp ELSE exp { Syntax.IfExp {test = $2; then' = $4; else' = Some $6; pos = Parsing.rhs_end_pos 1} }
| IF exp THEN exp { Syntax.IfExp {test = $2; then' = $4; else' = None; pos = Parsing.rhs_end_pos 1} }
| WHILE exp DO exp { Syntax.WhileExp {test = $2; body = $4; pos = Parsing.rhs_end_pos 1} }
| FOR ID ASSIGN exp TO exp DO exp { Syntax.ForExp {var = Syntax.Sym (Symbol.from_string $2); escape = ref true; lo = $4; hi = $6; body = $8; pos = Parsing.rhs_end_pos 1} }
| BREAK { Syntax.BreakExp (Parsing.rhs_end_pos 1) }
| LET decs IN expseqs END { Syntax.LetExp {decs = $2; body = $4; pos = Parsing.rhs_end_pos 1} }
| LPAREN exp RPAREN { $2 }
/* error */
| LPAREN error RPAREN { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.ErrorExp }
| ID LBRACE error RBRACE { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.ErrorExp }
| ID LBRACK error RBRACK OF exp { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.ArrayExp {typ = Syntax.Sym (Symbol.from_string $1); size = Syntax.ErrorExp ; init = $6; pos = Parsing.rhs_end_pos 1} }
| ID LBRACK exp error OF exp { let pos = Parsing.rhs_end_pos 4 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol);  Syntax.ArrayExp {typ = Syntax.Sym (Symbol.from_string $1); size = $3; init = $6; pos = Parsing.rhs_end_pos 1} }
| IF error THEN exp ELSE exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.IfExp {test = Syntax.ErrorExp; then' = $4; else' = Some $6; pos = Parsing.rhs_end_pos 1} }
| IF exp error exp ELSE exp { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.IfExp {test = $2; then' = $4; else' = Some $6; pos = Parsing.rhs_end_pos 1} }
| IF exp THEN error ELSE exp { let pos = Parsing.rhs_end_pos 4 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.IfExp {test = $2; then' = Syntax.ErrorExp; else' = Some $6; pos = Parsing.rhs_end_pos 1} }
| IF error THEN exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.IfExp {test = Syntax.ErrorExp; then' = $4; else' = None; pos = Parsing.rhs_end_pos 1} }
| WHILE error DO exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.WhileExp {test = Syntax.ErrorExp; body = $4; pos = Parsing.rhs_end_pos 1} }
| FOR error ASSIGN exp TO exp DO exp { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.ForExp {var = Syntax.ErrorSym; escape = ref true; lo = $4; hi = $6; body = $8; pos = Parsing.rhs_end_pos 1} }
| FOR ID error exp TO exp DO exp { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.ForExp {var = Syntax.Sym (Symbol.from_string $2); escape = ref true; lo = $4; hi = $6; body = $8; pos = Parsing.rhs_end_pos 1} }
| FOR ID ASSIGN error TO exp DO exp { let pos = Parsing.rhs_end_pos 4 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.ForExp {var = Syntax.Sym (Symbol.from_string $2); escape = ref true; lo = Syntax.ErrorExp ; hi = $6; body = $8; pos = Parsing.rhs_end_pos 1} }
| FOR ID ASSIGN exp error exp DO exp { let pos = Parsing.rhs_end_pos 5 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.ForExp {var = Syntax.Sym (Symbol.from_string $2); escape = ref true; lo = $4; hi = $6; body = $8; pos = Parsing.rhs_end_pos 1} }
| FOR ID ASSIGN exp TO error DO exp { let pos = Parsing.rhs_end_pos 6 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.ForExp {var = Syntax.Sym (Symbol.from_string $2); escape = ref true; lo = $4; hi = Syntax.ErrorExp ; body = $8; pos = Parsing.rhs_end_pos 1} }
| LET decs IN error END { let pos = Parsing.rhs_end_pos 4 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.LetExp {decs = $2; body = Syntax.ErrorExp ; pos = Parsing.rhs_end_pos 1} }

expseqs:
| exp expseq { Syntax.SeqExp ([($1, Parsing.rhs_end_pos 1)] @ $2) }
| { Syntax.SeqExp [] }

expseq:
| SEMICOLON exp expseq { [($2, Parsing.rhs_end_pos 2)] @ $3 }
| { [] }
/* error */
| SEMICOLON error expseq { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); [(Syntax.ErrorExp , Parsing.rhs_end_pos 2)] @ $3 }

seqs:
| exp SEMICOLON exp seq { Syntax.SeqExp ([($1, Parsing.rhs_end_pos 1); ($3, Parsing.rhs_end_pos 3)] @ $4) }
/* error */
| exp SEMICOLON error seq { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.SeqExp ([($1, Parsing.rhs_end_pos 1); (Syntax.ErrorExp , Parsing.rhs_end_pos 3)] @ $4) }

seq:
| SEMICOLON exp seq { [($2, Parsing.rhs_end_pos 2)] @ $3 }
| { [] }
/* error */
| SEMICOLON error seq { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); [(Syntax.ErrorExp , Parsing.rhs_end_pos 2)] @ $3}

params:
| exp param { $1 :: $2 }
| { [] }

param:
| COMMA exp param { $2 :: $3 }
| { [] }
/* error */
| COMMA error param { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); Syntax.ErrorExp  :: $3 }

fields:
| ID EQ exp field { (Syntax.Sym (Symbol.from_string $1), $3, Parsing.rhs_end_pos 1) :: $4 }
| { [] }
/* error */
| ID error exp field { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); (Syntax.Sym (Symbol.from_string $1), $3, Parsing.rhs_end_pos 1) :: $4 }
| ID EQ error field { let pos = Parsing.rhs_end_pos 3 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); (Syntax.Sym (Symbol.from_string $1), Syntax.ErrorExp , Parsing.rhs_end_pos 1) :: $4 }

field:
| COMMA ID EQ exp field { (Syntax.Sym (Symbol.from_string $2), $4, Parsing.rhs_end_pos 2) :: $5 }
| { [] }
/* error */
| COMMA error EQ exp field { let pos = Parsing.rhs_end_pos 2 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); (Syntax.ErrorSym, $4, Parsing.rhs_end_pos 2) :: $5 }
| COMMA ID EQ error field { let pos = Parsing.rhs_end_pos 4 in Printf.fprintf stdout "SyntaxError: invalid syntax in line %d at character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol); (Syntax.Sym (Symbol.from_string $2), Syntax.ErrorExp , Parsing.rhs_end_pos 2) :: $5 }
