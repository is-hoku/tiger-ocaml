open Semantic_action
open Semantic_action.Syntax
open Error

(* NOTE: Move to Translate module in chapter 6 *)
module Translate = struct
  type exp = unit
end

type expty = { exp : Translate.exp; ty : Types.ty }
type venv = Env.enventry Symbol_table.table
type tenv = Types.ty Symbol_table.table
type env = { venv : venv; tenv : tenv }

let rec trans_exp (venv, tenv, inside_loop, exp) =
  match exp with
  | VarExp v -> trans_var (venv, tenv, inside_loop, v)
  | NilExp -> { exp = (); ty = Types.Nil }
  | IntExp _ -> { exp = (); ty = Types.Int }
  | StringExp (_, _) -> { exp = (); ty = Types.String }
  | CallExp { func; args; pos } ->
      let ty = check_call venv tenv inside_loop func args pos in
      { exp = (); ty }
  | OpExp { left; oper = _; right; pos } ->
      let ty = check_op venv tenv inside_loop left right pos in
      { exp = (); ty }
  | RecordExp { fields; typ; pos } ->
      let ty = check_record venv tenv inside_loop fields typ pos in
      { exp = (); ty }
  | SeqExp exps -> check_seq venv tenv inside_loop exps
  | AssignExp { var; exp; pos } ->
      let ty = check_assign venv tenv inside_loop var exp pos in
      { exp = (); ty }
  | IfExp { test; then'; else'; pos } ->
      let ty = check_if venv tenv inside_loop test then' else' pos in
      { exp = (); ty }
  | WhileExp { test; body; pos } ->
      let ty = check_while venv tenv inside_loop test body pos in
      { exp = (); ty }
  | ForExp { var; escape; lo; hi; body; pos } ->
      let ty = check_for venv tenv inside_loop var escape lo hi body pos in
      { exp = (); ty }
  | BreakExp pos ->
      let ty = check_break inside_loop pos in
      { exp = (); ty }
  | LetExp { decs; body; _ } ->
      let ty = check_let venv tenv inside_loop decs body in
      { exp = (); ty }
  | ArrayExp { typ; size; init; pos } ->
      let ty = check_array venv tenv inside_loop typ size init pos in
      { exp = (); ty }
  | ErrorExp -> { exp = (); ty = Types.Error }

and trans_var (venv, tenv, inside_loop, var) =
  match var with
  | SimpleVar (sym, pos) -> (
      match sym with
      | Syntax.ErrorSym -> { exp = (); ty = Types.Error }
      | Syntax.Sym s -> (
          try
            match Symbol_table.look s venv with
            | Env.VarEntry { ty } -> { exp = (); ty = Types.actual_ty ty pos }
            | Env.FunEntry { formals = _; result = _ } ->
                print_error Name_error
                  (Printf.sprintf
                     "name %s is a function but expected a variable"
                     (Symbol.name s))
                  pos;
                { exp = (); ty = Types.Error }
          with Not_found ->
            print_error Name_error
              (Printf.sprintf "variable %s is not declared" (Symbol.name s))
              pos;
            { exp = (); ty = Types.Error }))
  | FieldVar (var, sym, pos) -> (
      (* Record field signature: var.sym *)
      match sym with
      | Syntax.ErrorSym -> { exp = (); ty = Types.Error }
      | Syntax.Sym s -> (
          let { exp = _; ty } = trans_var (venv, tenv, inside_loop, var) in
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
                print_error Name_error
                  (Printf.sprintf "field %s is not declared in record"
                     (Symbol.name s))
                  pos;
                { exp = (); ty = Types.Error })
          | _ ->
              print_error Type_error
                (Printf.sprintf
                   "variable %s has type %s but expected type record"
                   (Symbol.name s) (Types.string_of_ty ty))
                pos;
              { exp = (); ty = Types.Error }))
  | SubscriptVar (var, expr, pos) -> (
      (* Array subscript signature: var[expr] *)
      let { exp = _; ty = var_type } =
        trans_var (venv, tenv, inside_loop, var)
      in
      match var_type with
      | Types.Error -> { exp = (); ty = Types.Error }
      | Types.Array (array_type, _) -> (
          let { exp = _; ty = exp_type } =
            trans_exp (venv, tenv, inside_loop, expr)
          in
          match exp_type with
          | Types.Error -> { exp = (); ty = Types.Error }
          | Types.Int -> { exp = (); ty = array_type }
          | _ ->
              print_error Type_error
                (Printf.sprintf
                   "expression %s has type %s but expected type int"
                   (string_of_exp_raw expr)
                   (Types.string_of_ty exp_type))
                pos;
              { exp = (); ty = Types.Error })
      | _ ->
          print_error Type_error
            (Printf.sprintf "variable %s has type %s but expected type array"
               (string_of_var_raw var)
               (Types.string_of_ty var_type))
            pos;
          { exp = (); ty = Types.Error })

and check_call venv tenv inside_loop func args pos =
  match func with
  | Syntax.ErrorSym -> Types.Error
  | Syntax.Sym s -> (
      try
        match Symbol_table.look s venv with
        | Env.FunEntry { formals = expected_types; result = ty } -> (
            let actual_types =
              List.map
                (fun e -> (trans_exp (venv, tenv, inside_loop, e)).ty)
                args
            in
            let index = ref 0 in
            try
              List.iter2
                (fun expected actual ->
                  if not (Types.check expected actual) then
                    print_error Type_error
                      (Printf.sprintf
                         "argument %s has type %s but expected type%s"
                         (string_of_exp_raw (List.nth args !index))
                         (Types.string_of_ty (List.nth actual_types !index))
                         (Types.string_of_ty (List.nth expected_types !index)))
                      pos
                  else incr index)
                expected_types actual_types;
              ty
            with Invalid_argument _ ->
              print_error Type_error
                (Printf.sprintf "argument %s has type %s but expected type %s"
                   (string_of_exp_raw (List.nth args !index))
                   (Types.string_of_ty (List.nth actual_types !index))
                   (Types.string_of_ty (List.nth expected_types !index)))
                pos;
              Types.Error)
        | Env.VarEntry _ ->
            print_error Name_error
              (Printf.sprintf "name %s is a variable but expected a function"
                 (Symbol.name s))
              pos;
            Types.Error
      with Not_found ->
        print_error Name_error
          (Printf.sprintf "function %s is not declared" (Symbol.name s))
          pos;
        Types.Error)

and check_op venv tenv inside_loop left right pos =
  let { exp = _; ty = left_type } = trans_exp (venv, tenv, inside_loop, left) in
  let { exp = _; ty = right_type } =
    trans_exp (venv, tenv, inside_loop, right)
  in
  match (left_type, right_type) with
  | Types.Error, _ | _, Types.Error -> Types.Error
  | Types.Int, Types.Int -> Types.Int
  | Types.Int, other ->
      print_error Type_error
        (Printf.sprintf "expression %s has type %s but expected type int"
           (string_of_exp_raw right) (Types.string_of_ty other))
        pos;
      Types.Error
  | other, Types.Int ->
      print_error Type_error
        (Printf.sprintf "expression %s has type %s but expected type int"
           (string_of_exp_raw left) (Types.string_of_ty other))
        pos;
      Types.Error
  | other1, other2 ->
      print_error Type_error
        (Printf.sprintf "expression %s has type %s but expected type int"
           (string_of_exp_raw left)
           (Types.string_of_ty other1))
        pos;
      print_error Type_error
        (Printf.sprintf "expression %s has type %s but expected type int"
           (string_of_exp_raw right)
           (Types.string_of_ty other2))
        pos;
      Types.Error

and check_record venv tenv inside_loop fields typ pos =
  match find_type tenv typ pos with
  | Types.Record (record_fields, _un) as record_type -> (
      let expected_types = List.map snd record_fields in
      let actual_types =
        List.map
          (fun (_, expr, _) ->
            let { exp = _; ty } = trans_exp (venv, tenv, inside_loop, expr) in
            ty)
          fields
      in
      let index = ref 0 in
      try
        List.iter2
          (fun expected actual ->
            if not (Types.check expected actual) then
              print_error Type_error
                (Printf.sprintf "expression %s has type %s but expected type %s"
                   (let _, expr, _ = List.nth fields !index in
                    string_of_exp_raw expr)
                   (Types.string_of_ty (List.nth actual_types !index))
                   (Types.string_of_ty (List.nth expected_types !index)))
                pos
            else incr index)
          expected_types actual_types;
        record_type
      with Invalid_argument _ ->
        print_error Type_error
          (Printf.sprintf "expression %s has type %s but expected type %s"
             (let _, expr, _ = List.nth fields !index in
              string_of_exp_raw expr)
             (Types.string_of_ty (List.nth actual_types !index))
             (Types.string_of_ty (List.nth expected_types !index)))
          pos;
        Types.Error)
  | other -> (
      match typ with
      | Syntax.ErrorSym -> Types.Error
      | Syntax.Sym s ->
          print_error Type_error
            (Printf.sprintf "name %s is type %s but expected type record"
               (Symbol.name s) (Types.string_of_ty other))
            pos;
          Types.Error)

and check_seq venv tenv inside_loop exps =
  List.fold_left
    (fun _ (expr, _) ->
      match expr with
      | Syntax.BreakExp _ -> trans_exp (venv, tenv, false, expr)
      | _ -> trans_exp (venv, tenv, inside_loop, expr))
    { exp = (); ty = Types.Unit }
    exps

and check_assign venv tenv inside_loop var exp pos =
  let { exp = _; ty = var_type } = trans_var (venv, tenv, inside_loop, var) in
  let { exp = _; ty = exp_type } = trans_exp (venv, tenv, inside_loop, exp) in
  if not (Types.check var_type exp_type) then (
    print_error Type_error
      (Printf.sprintf "expression %s has type %s but expected type %s"
         (string_of_exp_raw exp)
         (Types.string_of_ty exp_type)
         (Types.string_of_ty var_type))
      pos;
    Types.Error)
  else Types.Unit

and check_if venv tenv inside_loop test then' else' pos =
  match else' with
  | None ->
      let { exp = _; ty = test_ty } =
        trans_exp (venv, tenv, inside_loop, test)
      in
      if not (Types.check Types.Int test_ty) then (
        print_error Type_error
          (Printf.sprintf "expression %s has type %s but expected type int"
             (string_of_exp_raw test)
             (Types.string_of_ty test_ty))
          pos;
        Types.Error)
      else
        let { exp = _; ty = then_ty } =
          trans_exp (venv, tenv, inside_loop, then')
        in
        if not (Types.check Types.Unit then_ty) then (
          print_error Type_error
            (Printf.sprintf "expression %s has type %s but expected type unit"
               (string_of_exp_raw then')
               (Types.string_of_ty then_ty))
            pos;
          Types.Error)
        else Types.Unit
  | Some else_exp ->
      let { exp = _; ty = test_ty } =
        trans_exp (venv, tenv, inside_loop, test)
      in
      if not (Types.check Types.Int test_ty) then (
        print_error Type_error
          (Printf.sprintf "expression %s has type %s but expected type int"
             (string_of_exp_raw test)
             (Types.string_of_ty test_ty))
          pos;
        Types.Error)
      else
        let { exp = _; ty = then_ty } =
          trans_exp (venv, tenv, inside_loop, then')
        in
        let { exp = _; ty = else_ty } =
          trans_exp (venv, tenv, inside_loop, else_exp)
        in
        if not (Types.check then_ty else_ty) then (
          print_error Type_error
            (Printf.sprintf "expression %s and %s has different types %s and %s"
               (string_of_exp_raw then')
               (string_of_exp_raw else_exp)
               (Types.string_of_ty then_ty)
               (Types.string_of_ty else_ty))
            pos;
          Types.Error)
        else else_ty

and check_while venv tenv inside_loop test body pos =
  let { exp = _; ty = test_ty } = trans_exp (venv, tenv, inside_loop, test) in
  if not (Types.check Types.Int test_ty) then (
    print_error Type_error
      (Printf.sprintf "expression %s has type %s but expected type int"
         (string_of_exp_raw test)
         (Types.string_of_ty test_ty))
      pos;
    Types.Error)
  else
    let { exp = _; ty = body_ty } = trans_exp (venv, tenv, true, body) in
    if not (Types.check Types.Unit body_ty) then (
      print_error Type_error
        (Printf.sprintf "expression %s has type %s but expected type unit"
           (string_of_exp_raw body)
           (Types.string_of_ty body_ty))
        pos;
      Types.Error)
    else Types.Unit

and check_for venv tenv inside_loop var escape lo hi body pos =
  let { exp = _; ty = lo_ty } = trans_exp (venv, tenv, inside_loop, lo) in
  if not (Types.check Types.Int lo_ty) then (
    print_error Type_error
      (Printf.sprintf "expression %s has type %s but expected type int"
         (string_of_exp_raw lo) (Types.string_of_ty lo_ty))
      pos;
    Types.Error)
  else
    let { exp = _; ty = hi_ty } = trans_exp (venv, tenv, inside_loop, hi) in
    if not (Types.check Types.Int hi_ty) then (
      print_error Type_error
        (Printf.sprintf "expression %s has type %s but expected type int"
           (string_of_exp_raw hi) (Types.string_of_ty hi_ty))
        pos;
      Types.Error)
    else
      let { venv = new_venv; tenv = _ } =
        trans_dec
          ( venv,
            tenv,
            inside_loop,
            VarDec { name = var; escape; typ = None; init = lo; pos } )
      in
      let { exp = _; ty = body_ty } = trans_exp (new_venv, tenv, true, body) in
      if not (Types.check Types.Unit body_ty) then (
        print_error Type_error
          (Printf.sprintf "expression %s has type %s but expected type unit"
             (string_of_exp_raw body)
             (Types.string_of_ty body_ty))
          pos;
        Types.Error)
      else Types.Unit

and check_break inside_loop pos =
  if inside_loop then Types.Unit
  else (
    print_error Name_error "unexpected break" pos;
    Types.Error)

and check_let venv tenv inside_loop decs body =
  let { venv = new_venv; tenv = new_tenv } =
    List.fold_left
      (fun { venv = venv'; tenv = tenv' } dec ->
        trans_dec (venv', tenv', inside_loop, dec))
      { venv; tenv } decs
  in
  let { exp = _; ty } = trans_exp (new_venv, new_tenv, inside_loop, body) in
  ty

and check_array venv tenv inside_loop typ size init pos =
  match typ with
  | Syntax.ErrorSym -> Types.Error
  | Syntax.Sym s -> (
      let ty = find_type tenv typ pos in
      match ty with
      | Types.Array (t, u) ->
          let { exp = _; ty = init_ty } =
            trans_exp (venv, tenv, inside_loop, init)
          in
          if not (Types.check t init_ty) then (
            print_error Type_error
              (Printf.sprintf
                 "expression %s and %s has different types %s and %s"
                 (Symbol.name s)
                 (Syntax.string_of_exp_raw init)
                 (Types.string_of_ty t)
                 (Types.string_of_ty init_ty))
              pos;
            Types.Error)
          else
            let { exp = _; ty = size_ty } =
              trans_exp (venv, tenv, inside_loop, size)
            in
            if not (Types.check Types.Int size_ty) then (
              print_error Type_error
                (Printf.sprintf
                   "expression %s has type %s but expected type int"
                   (Syntax.string_of_exp_raw size)
                   (Types.string_of_ty size_ty))
                pos;
              Types.Error)
            else Types.Array (t, u)
      | _ ->
          print_error Type_error
            (Printf.sprintf "name %s has type %s but expected type array"
               (Symbol.name s) (Types.string_of_ty ty))
            pos;
          Types.Error)

and trans_dec (venv, tenv, inside_loop, dec) =
  match dec with
  | VarDec { name; escape; typ; init; pos } ->
      trans_var_dec venv tenv inside_loop name escape typ init pos
  | FunctionDec f -> trans_fun_dec venv tenv inside_loop f
  | TypeDec t -> trans_type_dec venv tenv t

and trans_var_dec venv tenv inside_loop name _ typ init pos =
  match typ with
  | Some (sym, pos) -> (
      match name with
      | Syntax.ErrorSym -> { venv; tenv }
      | Syntax.Sym var_name ->
          let expected_ty = find_type tenv sym pos in
          let { exp = _; ty = init_ty } =
            trans_exp (venv, tenv, inside_loop, init)
          in
          if not (Types.check_record_nil expected_ty init_ty) then (
            print_error Type_error
              (Printf.sprintf "expression %s has type %s but expected type %s"
                 (Symbol.name var_name)
                 (Types.string_of_ty init_ty)
                 (Types.string_of_ty expected_ty))
              pos;
            { venv; tenv })
          else
            let new_venv =
              Symbol_table.enter var_name
                (Env.VarEntry { ty = expected_ty })
                venv
            in
            { venv = new_venv; tenv })
  | None -> (
      match name with
      | Syntax.ErrorSym -> { venv; tenv }
      | Syntax.Sym var_name ->
          let { exp = _; ty = init_ty } =
            trans_exp (venv, tenv, inside_loop, init)
          in
          if Types.check Types.Nil init_ty then (
            print_error Type_error "required type declared variable" pos;
            { venv; tenv })
          else
            let new_venv =
              Symbol_table.enter var_name (Env.VarEntry { ty = init_ty }) venv
            in
            { venv = new_venv; tenv })

and find_type tenv typ pos =
  match typ with
  | Syntax.ErrorSym -> Types.Error
  | Syntax.Sym t -> (
      try
        let ty = Symbol_table.look t tenv in
        Types.actual_ty ty pos
      with Not_found ->
        print_error Name_error
          (Printf.sprintf "type %s is not declared" (Symbol.name t))
          pos;
        Types.Error)

and trans_fun_header { venv; tenv }
    { fname; params; result; body = _; fpos = _ } =
  let param_types =
    List.map
      (fun { name = _; escape = _; typ; pos } -> find_type tenv typ pos)
      params
  in
  let result_type =
    match result with
    | None -> Types.Unit
    | Some typ -> find_type tenv (fst typ) (snd typ)
  in
  match fname with
  | Syntax.ErrorSym -> { venv; tenv }
  | Syntax.Sym name ->
      let new_venv =
        Symbol_table.enter name
          (Env.FunEntry { formals = param_types; result = result_type })
          venv
      in
      { venv = new_venv; tenv }

and enter_field { venv; tenv } { name; escape = _; typ; pos } =
  match name with
  | Syntax.ErrorSym -> { venv; tenv }
  | Syntax.Sym s ->
      let t = find_type tenv typ pos in
      let new_venv = Symbol_table.enter s (Env.VarEntry { ty = t }) venv in
      { venv = new_venv; tenv }

and trans_fun_dec venv tenv inside_loop f =
  let { venv = new_venv; tenv = _ } =
    List.fold_left trans_fun_header { venv; tenv } f
  in
  List.iter
    (fun { fname; params; result; body; fpos } ->
      let { venv = new_venv'; tenv = _ } =
        List.fold_left enter_field { venv = new_venv; tenv } params
      in
      let { exp = _; ty = body_ty } =
        trans_exp (new_venv', tenv, inside_loop, body)
      in
      let result_ty =
        match result with
        | None -> Types.Unit
        | Some sym -> find_type tenv (fst sym) (snd sym)
      in
      match fname with
      | Syntax.ErrorSym -> ()
      | Syntax.Sym name ->
          if not (Types.check body_ty result_ty) then
            print_error Type_error
              (Printf.sprintf "function %s return type %s but expected type %s"
                 (Symbol.name name)
                 (Types.string_of_ty body_ty)
                 (Types.string_of_ty result_ty))
              fpos)
    f;
  { venv = new_venv; tenv }

and trans_type_header { venv; tenv } { tname; ty = _; tpos = _ } =
  match tname with
  | Syntax.ErrorSym -> { venv; tenv }
  | Syntax.Sym name ->
      let new_tenv =
        Symbol_table.enter name (Types.Name (name, ref None)) tenv
      in
      { venv; tenv = new_tenv }

and trans_type_dec venv tenv t =
  let { venv = _; tenv = new_tenv } =
    List.fold_left trans_type_header { venv; tenv } t
  in
  List.iter
    (fun { tname; ty; tpos } ->
      match tname with
      | Syntax.ErrorSym -> ()
      | Syntax.Sym type_name -> (
          try
            let t = Symbol_table.look type_name new_tenv in
            match t with
            | Types.Name (sym, typ) -> (
                match !typ with
                | None -> typ := Some (trans_ty (new_tenv, ty))
                | Some _ ->
                    print_error Name_error
                      (Printf.sprintf "type %s is already declared"
                         (Symbol.name sym))
                      tpos)
            | _ ->
                print_error Name_error
                  (Printf.sprintf "type %s is already declared"
                     (Symbol.name type_name))
                  tpos
          with Not_found ->
            print_error Name_error
              (Printf.sprintf "type %s is not declared" (Symbol.name type_name))
              tpos))
    t;
  { venv; tenv = new_tenv }

and trans_ty (tenv, ty) =
  match ty with
  | NameTy (sym, pos) -> find_type tenv sym pos
  | RecordTy fields ->
      let record_fields =
        List.map
          (fun { name; escape = _; typ; pos } ->
            match name with
            | Syntax.ErrorSym -> (Symbol.dummy, Types.Error)
            | Syntax.Sym field_name ->
                let field_type = find_type tenv typ pos in
                (field_name, field_type))
          fields
      in
      Types.Record (record_fields, ref ())
  | ArrayTy (sym, pos) ->
      let array_type = find_type tenv sym pos in
      Types.Array (array_type, ref ())
