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
  | ErrorExp

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

let string_of_symbol = function
  | Sym t -> Symbol.name t
  | ErrorSym -> Printf.sprintf "error"

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
  | ErrorExp -> "ErrorExp"

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

(* string_of_{type-name}_raw functions return a string as it appears on the source code *)
let string_of_symbol_raw = function
  | Sym t -> Symbol.name t
  | ErrorSym -> Printf.sprintf "error"

let rec string_of_var_raw = function
  | SimpleVar (s, _) -> string_of_symbol_raw s
  | FieldVar (v, s, _) ->
      Printf.sprintf "%s.%s" (string_of_var_raw v) (string_of_symbol_raw s)
  | SubscriptVar (v, e, _) ->
      Printf.sprintf "%s[%s]" (string_of_var_raw v) (string_of_exp_raw e)

and string_of_record_field_raw (s, e, _) =
  Printf.sprintf "%s = %s" (string_of_symbol_raw s) (string_of_exp_raw e)

and string_of_else_exp_raw = function
  | None -> ""
  | Some a -> "else " ^ string_of_exp_raw a

and string_of_option_type_raw = function
  | None -> ""
  | Some (s, _) -> Printf.sprintf ": %s " (string_of_symbol_raw s)

and string_of_exp_raw = function
  | VarExp e -> string_of_var_raw e
  | NilExp -> "nil"
  | IntExp e -> string_of_int e
  | StringExp (s, _) -> s
  | CallExp { func; args; _ } ->
      Printf.sprintf "%s(%s)"
        (string_of_symbol_raw func)
        (String.concat ", " (List.map string_of_exp_raw args))
  | OpExp { left; oper; right; _ } ->
      Printf.sprintf "%s %s %s" (string_of_exp_raw left)
        (string_of_oper_raw oper) (string_of_exp_raw right)
  | RecordExp { fields; typ; _ } ->
      Printf.sprintf "%s{%s}" (string_of_symbol_raw typ)
        (String.concat ", " (List.map string_of_record_field_raw fields))
  | SeqExp e -> String.concat "; " (List.map string_of_exp_raw (List.map fst e))
  | AssignExp { var; exp; _ } ->
      Printf.sprintf "%s := %s" (string_of_var_raw var) (string_of_exp_raw exp)
  | IfExp { test; then'; else'; _ } ->
      Printf.sprintf "if %s then %s %s" (string_of_exp_raw test)
        (string_of_exp_raw then')
        (string_of_else_exp_raw else')
  | WhileExp { test; body; _ } ->
      Printf.sprintf "white %s do %s" (string_of_exp_raw test)
        (string_of_exp_raw body)
  | ForExp { var; escape = _; lo; hi; body; _ } ->
      Printf.sprintf "for %s := %s to %s do %s" (string_of_symbol_raw var)
        (string_of_exp_raw lo) (string_of_exp_raw hi) (string_of_exp_raw body)
  | BreakExp _ -> "break"
  | LetExp { decs; body; _ } ->
      Printf.sprintf "let %s in %s end"
        (String.concat " " (List.map string_of_dec_raw decs))
        (string_of_exp_raw body)
  | ArrayExp { typ; size; init; _ } ->
      Printf.sprintf "%s [%s] of %s" (string_of_symbol_raw typ)
        (string_of_exp_raw size) (string_of_exp_raw init)
  | ErrorExp -> "error"

and string_of_ty_raw = function
  | NameTy (s, _) -> string_of_symbol_raw s
  | RecordTy r ->
      Printf.sprintf "{%s}"
        (String.concat "; " (List.map string_of_field_raw r))
  | ArrayTy (s, _) -> Printf.sprintf "array of %s" (string_of_symbol s)

and string_of_dec_raw = function
  | FunctionDec d -> String.concat " " (List.map string_of_fundec_raw d)
  | VarDec { name; escape = _; typ; init; _ } ->
      Printf.sprintf "var %s %s:= %s"
        (string_of_symbol_raw name)
        (string_of_option_type_raw typ)
        (string_of_exp_raw init)
  | TypeDec d -> String.concat " " (List.map string_of_typedec_raw d)

and string_of_oper_raw = function
  | PlusOp -> "+"
  | MinusOp -> "-"
  | TimesOp -> "*"
  | DivideOp -> "/"
  | EqOp -> "="
  | NeqOp -> "!="
  | LtOp -> "<"
  | LeOp -> "<="
  | GtOp -> ">"
  | GeOp -> ">="

and string_of_field_raw { name; escape = _; typ; _ } =
  Printf.sprintf "%s : %s"
    (string_of_symbol_raw name)
    (string_of_symbol_raw typ)

and string_of_fundec_raw { fname; params; result; body; _ } =
  Printf.sprintf "function %s (%s) %s= %s"
    (string_of_symbol_raw fname)
    (String.concat ", " (List.map string_of_field_raw params))
    (string_of_option_type_raw result)
    (string_of_exp_raw body)

and string_of_typedec_raw { tname; ty; _ } =
  Printf.sprintf "type %s = %s"
    (string_of_symbol_raw tname)
    (string_of_ty_raw ty)
