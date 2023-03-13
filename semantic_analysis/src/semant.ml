open Semantic_action
open Semantic_action.Syntax
open Error

(* NOTE: Move to Translate module in chapter 6 later *)
module Translate = struct
  type exp = unit
end

type expty = { exp : Translate.exp; ty : Types.ty }
type venv = Env.enventry Symbol_table.table
type tenv = Types.ty Symbol_table.table
type env = { venv : venv; tenv : tenv }

(*
let check_errorsym s (pos : pos) =
  match s with
  | Syntax.ErrorSym ->
      raise
        (Parse_error
           (Printf.sprintf "invalid syntax in line %d at character %d"
              pos.pos_lnum
              (pos.pos_cnum - pos.pos_bol)))
  | Syntax.Sym s -> s

let check_int ({ exp = _; ty = t }, (pos : pos)) =
  match t with
  | Types.Int -> ()
  | _ ->
      raise
        (Type_error
           (Printf.sprintf
              "\'%s\' has type %s but expected of type int in line %d at \
               character %d"
              "(exp)"
              (* NOTE: ↑ Replace this with exp of the argument *)
              (Types.string_of_ty t)
              pos.pos_lnum
              (pos.pos_cnum - pos.pos_bol)))
*)

let rec trans_exp venv tenv exp =
  match exp with
  | VarExp v -> trans_var venv tenv v
  | NilExp -> { exp = (); ty = Types.Nil }
  | IntExp _ -> { exp = (); ty = Types.Int }
  | StringExp (_, _) -> { exp = (); ty = Types.String }
  | CallExp { func = _; args = _; pos = _ } -> { exp = (); ty = Types.Int }
  | OpExp { left = _; oper = _; right = _; pos = _ } ->
      { exp = (); ty = Types.Int }
  | RecordExp { fields = _; typ = _; pos = _ } -> { exp = (); ty = Types.Int }
  | SeqExp _ -> { exp = (); ty = Types.Int }
  | AssignExp { var = _; exp = _; pos = _ } -> { exp = (); ty = Types.Int }
  | IfExp { test = _; then' = _; else' = _; pos = _ } ->
      { exp = (); ty = Types.Int }
  | WhileExp { test = _; body = _; pos = _ } -> { exp = (); ty = Types.Int }
  | ForExp { var = _; escape = _; lo = _; hi = _; body = _; pos = _ } ->
      { exp = (); ty = Types.Int }
  | BreakExp _ -> { exp = (); ty = Types.Int }
  | LetExp { decs = _; body = _; pos = _ } -> { exp = (); ty = Types.Int }
  | ArrayExp { typ = _; size = _; init = _; pos = _ } ->
      { exp = (); ty = Types.Int }
  | ErrorExp -> { exp = (); ty = Types.Error Types.Syntax_error }

and trans_var venv tenv var =
  match var with
  | SimpleVar (sym, pos) -> (
      match sym with
      | Syntax.ErrorSym -> { exp = (); ty = Types.Error Types.Syntax_error }
      | Syntax.Sym s -> (
          try
            match Symbol_table.look s venv with
            | Env.VarEntry { ty } -> { exp = (); ty = Types.actual_ty ty pos }
            | Env.FunEntry { formals = _; result = ty } ->
                {
                  exp = ();
                  ty =
                    Types.Error
                      (Types.Type_error
                         (Printf.sprintf
                            "name %s is a function but expected a variable in \
                             line %d at character %d"
                            (Types.string_of_ty ty) pos.pos_lnum
                            (pos.pos_cnum - pos.pos_bol)));
                }
          with Not_found ->
            {
              exp = ();
              ty =
                Types.Error
                  (Types.Name_error
                     (Printf.sprintf
                        "variable %s is not defined in line %d at character %d"
                        (Symbol.name s) pos.pos_lnum
                        (pos.pos_cnum - pos.pos_bol)));
            }))
  | FieldVar (v, sym, pos) -> (
      (* Record field signature: v.sym *)
      match sym with
      | Syntax.ErrorSym -> { exp = (); ty = Types.Error Types.Syntax_error }
      | Syntax.Sym s -> (
          let { exp = _; ty = t } = trans_var tenv venv v in
          match t with
          | Types.Error e -> { exp = (); ty = Types.Error e }
          | Types.Record (fields, _) -> (
              try
                let _, ftype =
                  (* Find sym in the field of Record *)
                  List.find (fun (f, _) -> Symbol.name f = Symbol.name s) fields
                in
                { exp = (); ty = ftype }
              with Not_found ->
                {
                  exp = ();
                  ty =
                    Types.Error
                      (Types.Name_error
                         (Printf.sprintf
                            "field %s is not undeclared in record in line %d \
                             at character %d"
                            (Symbol.name s) pos.pos_lnum
                            (pos.pos_cnum - pos.pos_bol)));
                })
          | _ ->
              {
                exp = ();
                ty =
                  Types.Error
                    (Types.Type_error
                       (Printf.sprintf
                          "variable %s has type %s but expected type Record in \
                           line %d at character %d"
                          (Symbol.name s) (Types.string_of_ty t) pos.pos_lnum
                          (pos.pos_cnum - pos.pos_bol)));
              }))
  | SubscriptVar (v, e, pos) -> (
      (* Array subscript signature: v[e] *)
      let { exp = _; ty = var_type } = trans_var tenv venv v in
      match var_type with
      | Types.Error e -> { exp = (); ty = Types.Error e }
      | Types.Array (array_type, _) -> (
          let { exp = _; ty = exp_type } = trans_exp tenv venv e in
          match exp_type with
          | Types.Error e -> { exp = (); ty = Types.Error e }
          | Types.Int -> { exp = (); ty = array_type }
          | _ ->
              {
                exp = ();
                ty =
                  Types.Error
                    (Types.Type_error
                       (Printf.sprintf
                          "expression %s has type %s but expected type int not \
                           in line %d at character %d"
                          (string_of_exp e)
                          (Types.string_of_ty exp_type)
                          pos.pos_lnum
                          (pos.pos_cnum - pos.pos_bol)));
              })
      | _ ->
          {
            exp = ();
            ty =
              Types.Error
                (Types.Type_error
                   (Printf.sprintf
                      "variable %s has type %s but expected type Array in line \
                       %d at character %d"
                      (string_of_var v)
                      (Types.string_of_ty var_type)
                      pos.pos_lnum
                      (pos.pos_cnum - pos.pos_bol)));
          })
