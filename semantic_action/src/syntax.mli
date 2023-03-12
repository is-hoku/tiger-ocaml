type pos = Lexing.position
type symbol = Sym of Symbol.t | ErrorSym

type var =
  | SimpleVar of symbol * pos
  | FieldVar of var * symbol * pos
  | SubscriptVar of var * exp * pos

and exp =
  | VarExp of var
  | NilExp
  | IntExp of int
  | StringExp of string * pos
  | CallExp of { func : symbol; args : exp list; pos : pos }
  | OpExp of { left : exp; oper : oper; right : exp; pos : pos }
  | RecordExp of { fields : (symbol * exp * pos) list; typ : symbol; pos : pos }
  | SeqExp of (exp * pos) list
  | AssignExp of { var : var; exp : exp; pos : pos }
  | IfExp of { test : exp; then' : exp; else' : exp option; pos : pos }
  | WhileExp of { test : exp; body : exp; pos : pos }
  | ForExp of {
      var : symbol;
      escape : bool ref;
      lo : exp;
      hi : exp;
      body : exp;
      pos : pos;
    }
  | BreakExp of pos
  | LetExp of { decs : dec list; body : exp; pos : pos }
  | ArrayExp of { typ : symbol; size : exp; init : exp; pos : pos }
  | ErrorExp of pos

and ty =
  | NameTy of symbol * pos
  | RecordTy of field list
  | ArrayTy of symbol * pos

and dec =
  | FunctionDec of fundec list
  | VarDec of {
      name : symbol;
      escape : bool ref;
      typ : (symbol * pos) option;
      init : exp;
      pos : pos;
    }
  | TypeDec of typedec list

and oper =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp

and field = { name : symbol; escape : bool ref; typ : symbol; pos : pos }

and fundec = {
  fname : symbol;
  params : field list;
  result : (symbol * pos) option;
  body : exp;
  fpos : pos;
}

and typedec = { tname : symbol; ty : ty; tpos : pos }

val string_of_symbol : symbol -> string
val string_of_pos : pos -> string
val string_of_var : var -> string
val string_of_exp_pos : exp * pos -> string
val string_of_exp_option : exp option -> string
val string_of_symbol_pos_option : (symbol * pos) option -> string
val string_of_exp : exp -> string
val string_of_ty : ty -> string
val string_of_dec : dec -> string
val string_of_oper : oper -> string
val string_of_symbol_exp_pos : symbol * exp * pos -> string
val string_of_field : field -> string
val string_of_fundec : fundec -> string
val string_of_typedec : typedec -> string