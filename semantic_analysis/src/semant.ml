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

let rec trans_exp (venv, tenv, exp) =
  match exp with
  | VarExp v -> trans_var (venv, tenv, v)
  | NilExp -> { exp = (); ty = Types.Nil }
  | IntExp _ -> { exp = (); ty = Types.Int }
  | StringExp (_, _) -> { exp = (); ty = Types.String }
  | CallExp { func; args; pos } ->
      let ty = check_call venv tenv func args pos in
      { exp = (); ty }
  | OpExp { left; oper = _; right; pos } ->
      let ty = check_op venv tenv left right pos in
      { exp = (); ty }
  | RecordExp { fields; typ; pos } ->
      let ty = check_record venv tenv fields typ pos in
      { exp = (); ty }
  | SeqExp exps ->
      List.fold_left
        (fun _ (expr, _) -> trans_exp (venv, tenv, expr))
        { exp = (); ty = Types.Unit }
        exps
  | AssignExp { var; exp; pos } ->
      let ty = check_assign venv tenv var exp pos in
      { exp = (); ty }
  | IfExp { test = _; then' = _; else' = _; pos = _ } ->
      { exp = (); ty = Types.Int }
  | WhileExp { test = _; body = _; pos = _ } -> { exp = (); ty = Types.Int }
  | ForExp { var = _; escape = _; lo = _; hi = _; body = _; pos = _ } ->
      { exp = (); ty = Types.Int }
  | BreakExp _ -> { exp = (); ty = Types.Unit }
  | LetExp { decs = _; body = _; pos = _ } -> { exp = (); ty = Types.Int }
  | ArrayExp { typ = _; size = _; init = _; pos = _ } ->
      { exp = (); ty = Types.Int }
  | ErrorExp -> { exp = (); ty = Types.Error }

and trans_var (venv, tenv, var) =
  match var with
  | SimpleVar (sym, pos) -> (
      match sym with
      | Syntax.ErrorSym -> { exp = (); ty = Types.Error }
      | Syntax.Sym s -> (
          try
            match Symbol_table.look s venv with
            | Env.VarEntry { ty } -> { exp = (); ty = Types.actual_ty ty pos }
            | Env.FunEntry { formals = _; result = ty } ->
                Printf.fprintf stdout
                  "TypeError: name %s is a function but expected a variable in \
                   line %d at character %d"
                  (Types.string_of_ty ty) pos.pos_lnum
                  (pos.pos_cnum - pos.pos_bol);
                { exp = (); ty = Types.Error }
          with Not_found ->
            Printf.fprintf stdout
              "NameError: variable %s is not declared in line %d at character \
               %d"
              (Symbol.name s) pos.pos_lnum
              (pos.pos_cnum - pos.pos_bol);
            { exp = (); ty = Types.Error }))
  | FieldVar (var, sym, pos) -> (
      (* Record field signature: var.sym *)
      match sym with
      | Syntax.ErrorSym -> { exp = (); ty = Types.Error }
      | Syntax.Sym s -> (
          let { exp = _; ty } = trans_var (venv, tenv, var) in
          match ty with
          | Types.Error -> { exp = (); ty = Types.Error }
          | Types.Record (fields, _) -> (
              try
                let _, field_type =
                  (* Find sym in the field of Record *)
                  List.find (fun (f, _) -> Symbol.name f = Symbol.name s) fields
                in
                { exp = (); ty = field_type }
              with Not_found ->
                Printf.fprintf stdout
                  "NameError: field %s is not declared in record in line %d at \
                   character %d"
                  (Symbol.name s) pos.pos_lnum
                  (pos.pos_cnum - pos.pos_bol);
                { exp = (); ty = Types.Error })
          | _ ->
              Printf.fprintf stdout
                "TypeError: variable %s has type %s but expected type Record \
                 in line %d at character %d"
                (Symbol.name s) (Types.string_of_ty ty) pos.pos_lnum
                (pos.pos_cnum - pos.pos_bol);
              { exp = (); ty = Types.Error }))
  | SubscriptVar (var, expr, pos) -> (
      (* Array subscript signature: var[expr] *)
      let { exp = _; ty = var_type } = trans_var (venv, tenv, var) in
      match var_type with
      | Types.Error -> { exp = (); ty = Types.Error }
      | Types.Array (array_type, _) -> (
          let { exp = _; ty = exp_type } = trans_exp (venv, tenv, expr) in
          match exp_type with
          | Types.Error -> { exp = (); ty = Types.Error }
          | Types.Int -> { exp = (); ty = array_type }
          | _ ->
              Printf.fprintf stdout
                "TypeError: expression %s has type %s but expected type int \
                 not in line %d at character %d"
                (string_of_exp expr)
                (Types.string_of_ty exp_type)
                pos.pos_lnum
                (pos.pos_cnum - pos.pos_bol);
              { exp = (); ty = Types.Error })
      | _ ->
          Printf.fprintf stdout
            "TypeError: variable %s has type %s but expected type Array in \
             line %d at character %d"
            (string_of_var var)
            (Types.string_of_ty var_type)
            pos.pos_lnum
            (pos.pos_cnum - pos.pos_bol);
          { exp = (); ty = Types.Error })

and check_call venv tenv func args pos =
  match func with
  | Syntax.ErrorSym -> Types.Error
  | Syntax.Sym s -> (
      try
        match Symbol_table.look s venv with
        | Env.FunEntry { formals = expected_types; result = ty } -> (
            let actual_types =
              List.map (fun e -> (trans_exp (venv, tenv, e)).ty) args
            in
            let index = ref 0 in
            try
              List.iter2
                (fun expected actual ->
                  if not (Types.check expected actual) then
                    let error_message =
                      Printf.sprintf
                        "TypeError: argument %s has type %s but expected %s in \
                         line %d at character %d"
                        (string_of_exp (List.nth args !index))
                        (Types.string_of_ty (List.nth actual_types !index))
                        (Types.string_of_ty (List.nth expected_types !index))
                        pos.pos_lnum
                        (pos.pos_cnum - pos.pos_bol)
                    in
                    raise (Invalid_argument error_message)
                  else incr index)
                expected_types actual_types;
              ty
            with Invalid_argument e ->
              Printf.fprintf stdout "%s" e;
              Types.Error)
        | Env.VarEntry _ ->
            Printf.fprintf stdout
              "NameError: name %s is a variable but expected a function in \
               line %d at character %d"
              (Symbol.name s) pos.pos_lnum
              (pos.pos_cnum - pos.pos_bol);
            Types.Error
      with Not_found ->
        Printf.fprintf stdout
          "NameError: function %s is not declared in line %d at character %d"
          (Symbol.name s) pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol);
        Types.Error)

and check_op venv tenv left right pos =
  let { exp = _; ty = left_type } = trans_exp (venv, tenv, left) in
  let { exp = _; ty = right_type } = trans_exp (venv, tenv, right) in
  match (left_type, right_type) with
  | Types.Error, _ | _, Types.Error -> Types.Int
  | Types.Int, Types.Int -> Types.Int
  | Types.Int, other ->
      Printf.fprintf stdout
        "TypeError: expression %s has type %s but expected int in line %d at \
         character %d"
        (string_of_exp right) (Types.string_of_ty other) pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol);
      Types.Error
  | other, Types.Int ->
      Printf.fprintf stdout
        "TypeError: expression %s has type %s but expected int in line %d at \
         character %d"
        (string_of_exp left) (Types.string_of_ty other) pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol);
      Types.Error
  | other1, other2 ->
      Printf.fprintf stdout
        "TypeError: expression %s has type %s but expected int in line %d at \
         character %d"
        (string_of_exp left)
        (Types.string_of_ty other1)
        pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol);
      Printf.fprintf stdout
        "TypeError: expression %s has type %s but expected int in line %d at \
         character %d"
        (string_of_exp left)
        (Types.string_of_ty other2)
        pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol);
      Types.Error

and check_record venv tenv fields typ pos =
  match typ with
  | Syntax.ErrorSym -> Types.Error
  | Syntax.Sym s -> (
      try
        match Symbol_table.look s tenv with
        | Types.Record (record_fields, _un) as record_type -> (
            let expected_types = List.map snd record_fields in
            let actual_types =
              List.map
                (fun (_, expr, _) ->
                  let { exp = _; ty } = trans_exp (venv, tenv, expr) in
                  ty)
                fields
            in
            let index = ref 0 in
            try
              List.iter2
                (fun expected actual ->
                  if not (Types.check expected actual) then
                    let error_message =
                      Printf.sprintf
                        "TypeError: expression %s has type %s but expected %s \
                         in line %d at character %d"
                        (let _, expr, _ = List.nth fields !index in
                         string_of_exp expr)
                        (Types.string_of_ty (List.nth actual_types !index))
                        (Types.string_of_ty (List.nth expected_types !index))
                        pos.pos_lnum
                        (pos.pos_cnum - pos.pos_bol)
                    in
                    raise (Invalid_argument error_message)
                  else incr index)
                expected_types actual_types;
              record_type
            with Invalid_argument e ->
              Printf.fprintf stdout "%s" e;
              Types.Error)
        | other ->
            Printf.fprintf stdout
              "TypeError: name %s is type %s but expected type record in line \
               %d at character %d"
              (Symbol.name s) (Types.string_of_ty other) pos.pos_lnum
              (pos.pos_cnum - pos.pos_bol);
            Types.Error
      with Not_found ->
        Printf.fprintf stdout
          "TypeError: type %s is not declared in line %d at character %d"
          (Symbol.name s) pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol);
        Types.Error)
