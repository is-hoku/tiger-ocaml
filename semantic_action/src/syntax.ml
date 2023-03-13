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

let string_of_symbol = function Sym t -> Symbol.name t | ErrorSym -> "error"

let string_of_pos (p : pos) =
  Printf.sprintf "(%d, %d)" p.pos_lnum (p.pos_cnum - p.pos_bol)

let rec string_of_var = function
  | SimpleVar (s, p) ->
      Printf.sprintf "SimpleVar(%s, %s)" (string_of_symbol s) (string_of_pos p)
  | FieldVar (v, s, p) ->
      Printf.sprintf "FieldVar(%s, %s, %s)" (string_of_var v)
        (string_of_symbol s) (string_of_pos p)
  | SubscriptVar (v, e, p) ->
      Printf.sprintf "SubscriptVar(%s, %s, %s)" (string_of_var v)
        (string_of_exp e) (string_of_pos p)

and string_of_exp_pos (e, p) =
  Printf.sprintf "(%s, %s)" (string_of_exp e) (string_of_pos p)

and string_of_exp_option = function None -> "None" | Some a -> string_of_exp a

and string_of_symbol_pos_option = function
  | None -> "None"
  | Some (s, p) ->
      Printf.sprintf "(%s, %s)" (string_of_symbol s) (string_of_pos p)

and string_of_exp = function
  | VarExp e -> Printf.sprintf "VarExp(%s)" (string_of_var e)
  | NilExp -> "nil"
  | IntExp e -> "IntExp " ^ string_of_int e
  | StringExp (s, p) -> Printf.sprintf "StringExp(%s, %s)" s (string_of_pos p)
  | CallExp e ->
      Printf.sprintf "CallExp{func=%s, args=%s, pos=%s}"
        (string_of_symbol e.func)
        (Printf.sprintf "[%s]"
           (String.concat ", " (List.map string_of_exp e.args)))
        (string_of_pos e.pos)
  | OpExp e ->
      Printf.sprintf "OpExp{left=%s, oper=%s, right=%s, pos=%s}"
        (string_of_exp e.left) (string_of_oper e.oper) (string_of_exp e.right)
        (string_of_pos e.pos)
  | RecordExp e ->
      Printf.sprintf "RecordExp{fields=%s, typ=%s, pos=%s}"
        (Printf.sprintf "[%s]"
           (String.concat ", " (List.map string_of_symbol_exp_pos e.fields)))
        (string_of_symbol e.typ) (string_of_pos e.pos)
  | SeqExp e ->
      Printf.sprintf "SeqExp[%s]"
        (String.concat ", " (List.map string_of_exp_pos e))
  | AssignExp e ->
      Printf.sprintf "AssignExp{var=%s, exp=%s, pos=%s}" (string_of_var e.var)
        (string_of_exp e.exp) (string_of_pos e.pos)
  | IfExp e ->
      Printf.sprintf "IfExp{test=%s, then'=%s, else'=%s, pos=%s}"
        (string_of_exp e.test) (string_of_exp e.then')
        (string_of_exp_option e.else')
        (string_of_pos e.pos)
  | WhileExp e ->
      Printf.sprintf "WhileExp{test=%s, body=%s, pos=%s}" (string_of_exp e.test)
        (string_of_exp e.body) (string_of_pos e.pos)
  | ForExp e ->
      Printf.sprintf "ForExp{var=%s, escape=%s, lo=%s, hi=%s, body=%s, pos=%s}"
        (string_of_symbol e.var)
        (string_of_bool !(e.escape))
        (string_of_exp e.lo) (string_of_exp e.hi) (string_of_exp e.body)
        (string_of_pos e.pos)
  | BreakExp p -> Printf.sprintf "BreakExp(%s)" (string_of_pos p)
  | LetExp e ->
      Printf.sprintf "LetExp{decs=%s, body=%s, pos=%s}"
        (Printf.sprintf "[%s]"
           (String.concat ", " (List.map string_of_dec e.decs)))
        (string_of_exp e.body) (string_of_pos e.pos)
  | ArrayExp e ->
      Printf.sprintf "ArrayExp{typ=%s, size=%s, init=%s, pos=%s}"
        (string_of_symbol e.typ) (string_of_exp e.size) (string_of_exp e.init)
        (string_of_pos e.pos)
  | ErrorExp p -> Printf.sprintf "ErrorExp(%s)" (string_of_pos p)

and string_of_ty = function
  | NameTy (s, p) ->
      Printf.sprintf "NameTy(%s, %s)" (string_of_symbol s) (string_of_pos p)
  | RecordTy r ->
      Printf.sprintf "RecordTy[%s]"
        (String.concat ", " (List.map string_of_field r))
  | ArrayTy (s, p) ->
      Printf.sprintf "ArrayTy(%s, %s)" (string_of_symbol s) (string_of_pos p)

and string_of_dec = function
  | FunctionDec d ->
      Printf.sprintf "FunctionDec[%s]"
        (String.concat ", " (List.map string_of_fundec d))
  | VarDec d ->
      Printf.sprintf "VarDec{name=%s, escape=%s, typ=%s, init=%s, pos=%s}"
        (string_of_symbol d.name)
        (string_of_bool !(d.escape))
        (string_of_symbol_pos_option d.typ)
        (string_of_exp d.init) (string_of_pos d.pos)
  | TypeDec d ->
      Printf.sprintf "TypeDec[%s]"
        (String.concat ", " (List.map string_of_typedec d))

and string_of_oper = function
  | PlusOp -> "(+)"
  | MinusOp -> "(-)"
  | TimesOp -> "(*)"
  | DivideOp -> "(/)"
  | EqOp -> "(=)"
  | NeqOp -> "(!=)"
  | LtOp -> "(<)"
  | LeOp -> "(<=)"
  | GtOp -> "(>)"
  | GeOp -> "(>=)"

and string_of_symbol_exp_pos (s, e, p) =
  Printf.sprintf "(%s, %s, %s)" (string_of_symbol s) (string_of_exp e)
    (string_of_pos p)

and string_of_field t =
  Printf.sprintf "{name=%s, escape=%s, typ=%s, pos=%s}"
    (string_of_symbol t.name)
    (string_of_bool !(t.escape))
    (string_of_symbol t.typ) (string_of_pos t.pos)

and string_of_fundec t =
  Printf.sprintf "{name=%s, params=%s, result=%s, body=%s, pos=%s}"
    (string_of_symbol t.fname)
    (Printf.sprintf "[%s]"
       (String.concat ", " (List.map string_of_field t.params)))
    (string_of_symbol_pos_option t.result)
    (string_of_exp t.body) (string_of_pos t.fpos)

and string_of_typedec t =
  Printf.sprintf "{name=%s, ty=%s, pos=%s}" (string_of_symbol t.tname)
    (string_of_ty t.ty) (string_of_pos t.tpos)
