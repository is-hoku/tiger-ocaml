module Env' = Env
open Semantic_action
open Semantic_action.Syntax
open Semantic_analysis
open Semantic_analysis.Error

type expty = { exp : Translate.exp; ty : Types.ty }
type venv = Env'.enventry Symbol_table.table
type tenv = Types.ty Symbol_table.table
type env = { venv : venv; tenv : tenv }
type inside_loop = bool
type dec_error = bool

let rec trans_exp (venv, tenv, inside_loop, exp, level) =
  match exp with
  | VarExp v -> trans_var (venv, tenv, inside_loop, v, level)
  | NilExp -> { exp = (); ty = Types.Nil }
  | IntExp _ -> { exp = (); ty = Types.Int }
  | StringExp (_, _) -> { exp = (); ty = Types.String }
  | CallExp { func; args; pos } ->
      let ty = check_call venv tenv inside_loop func args pos level in
      { exp = (); ty }
  | OpExp { left; oper; right; pos } ->
      let ty = check_op venv tenv inside_loop left oper right pos level in
      { exp = (); ty }
  | RecordExp { fields; typ; pos } ->
      let ty = check_record venv tenv inside_loop fields typ pos level in
      { exp = (); ty }
  | SeqExp exps -> check_seq venv tenv inside_loop exps level
  | AssignExp { var; exp; pos } ->
      let ty = check_assign venv tenv inside_loop var exp pos level in
      { exp = (); ty }
  | IfExp { test; then'; else'; pos } ->
      let ty = check_if venv tenv inside_loop test then' else' pos level in
      { exp = (); ty }
  | WhileExp { test; body; pos } ->
      let ty = check_while venv tenv inside_loop test body pos level in
      { exp = (); ty }
  | ForExp { var; escape; lo; hi; body; pos } ->
      let ty =
        check_for venv tenv inside_loop var escape lo hi body pos level
      in
      { exp = (); ty }
  | BreakExp pos ->
      let ty = check_break inside_loop pos in
      { exp = (); ty }
  | LetExp { decs; body; _ } ->
      let ty = check_let venv tenv inside_loop decs body level in
      { exp = (); ty }
  | ArrayExp { typ; size; init; pos } ->
      let ty = check_array venv tenv inside_loop typ size init pos level in
      { exp = (); ty }
  | ErrorExp -> { exp = (); ty = Types.Error }

and trans_var (venv, tenv, inside_loop, var, level) =
  match var with
  | SimpleVar (sym, pos) -> (
      match sym with
      | Syntax.ErrorSym -> { exp = (); ty = Types.Error }
      | Syntax.Sym s -> (
          try
            match Symbol_table.look s venv with
            | Env'.VarEntry { access = _; ty } ->
                { exp = (); ty = Types.actual_ty ty pos }
            | Env'.FunEntry _ ->
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
          let { exp = _; ty } =
            trans_var (venv, tenv, inside_loop, var, level)
          in
          match Types.actual_ty ty pos with
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
        trans_var (venv, tenv, inside_loop, var, level)
      in
      match Types.actual_ty var_type pos with
      | Types.Error -> { exp = (); ty = Types.Error }
      | Types.Array (array_type, _) -> (
          let { exp = _; ty = exp_type } =
            trans_exp (venv, tenv, inside_loop, expr, level)
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

and check_call venv tenv inside_loop func args pos level =
  match func with
  | Syntax.ErrorSym -> Types.Error
  | Syntax.Sym s -> (
      try
        match Symbol_table.look s venv with
        | Env'.FunEntry
            { level = _; label = _; formals = expected_types; result = ty } -> (
            let actual_types =
              List.map
                (fun e -> (trans_exp (venv, tenv, inside_loop, e, level)).ty)
                args
            in
            let index = ref 0 in
            try
              let formals_ty =
                List.map2
                  (fun expected actual ->
                    if not (Types.check expected actual) then (
                      print_error Type_error
                        (Printf.sprintf
                           "argument %s has type %s but expected type%s"
                           (string_of_exp_raw (List.nth args !index))
                           (Types.string_of_ty (List.nth actual_types !index))
                           (Types.string_of_ty (List.nth expected_types !index)))
                        pos;
                      Types.Error)
                    else if expected = Types.Error || actual = Types.Error then
                      Types.Error
                    else (
                      incr index;
                      actual))
                  expected_types actual_types
              in
              if
                List.fold_left
                  (fun is_error ty ->
                    if is_error || ty = Types.Error then true else false)
                  false actual_types
                || List.fold_left
                     (fun is_error ty ->
                       if is_error || ty = Types.Error then true else false)
                     false formals_ty
              then Types.Error
              else ty
            with
            | Failure _ ->
                print_error Type_error
                  (Printf.sprintf
                     "the number of arguments for function %s is not correct"
                     (Symbol.name s))
                  pos;
                Types.Error
            | Invalid_argument _ ->
                print_error Type_error
                  (Printf.sprintf
                     "the number of arguments for function %s is not correct"
                     (Symbol.name s))
                  pos;
                Types.Error)
        | Env'.VarEntry _ ->
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

and check_op venv tenv inside_loop left oper right pos level =
  let { exp = _; ty = left_type } =
    trans_exp (venv, tenv, inside_loop, left, level)
  in
  let { exp = _; ty = right_type } =
    trans_exp (venv, tenv, inside_loop, right, level)
  in
  match oper with
  | PlusOp | MinusOp | TimesOp | DivideOp -> (
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
          Types.Error)
  | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp -> (
      match (left_type, right_type) with
      | Types.Error, _ | _, Types.Error -> Types.Error
      | Types.Int, Types.Int -> Types.Int
      | Types.String, Types.String -> Types.Int
      | Types.Record (_, _), Types.Nil | Types.Nil, Types.Record (_, _) ->
          Types.Int
      | (Types.Int as typ), other | (Types.String as typ), other ->
          print_error Type_error
            (Printf.sprintf "expression %s has type %s but expected type %s"
               (string_of_exp_raw right) (Types.string_of_ty other)
               (Types.string_of_ty typ))
            pos;
          Types.Error
      | other, (Types.Int as typ) | other, (Types.String as typ) ->
          print_error Type_error
            (Printf.sprintf "expression %s has type %s but expected type %s"
               (string_of_exp_raw left) (Types.string_of_ty other)
               (Types.string_of_ty typ))
            pos;
          Types.Error
      | other1, other2 ->
          print_error Type_error
            (Printf.sprintf
               "expression %s has type %s but expected type int or string"
               (string_of_exp_raw left)
               (Types.string_of_ty other1))
            pos;
          print_error Type_error
            (Printf.sprintf
               "expression %s has type %s but expected type int or string"
               (string_of_exp_raw right)
               (Types.string_of_ty other2))
            pos;
          Types.Error)

and check_record venv tenv inside_loop fields typ pos level =
  match typ with
  | Syntax.ErrorSym -> Types.Error
  | Syntax.Sym s -> (
      match find_type tenv typ pos with
      | Types.Error -> Types.Error
      | Types.Record (record_fields, _un) as record_type -> (
          let expected_types = List.map snd record_fields in
          let actual_types =
            List.map
              (fun (_, expr, _) ->
                let { exp = _; ty } =
                  trans_exp (venv, tenv, inside_loop, expr, level)
                in
                ty)
              fields
          in
          let index = ref 0 in
          try
            let fields_ty =
              List.map2
                (fun expected actual ->
                  if not (Types.check expected actual) then (
                    print_error Type_error
                      (Printf.sprintf
                         "expression %s has type %s but expected type %s"
                         (let _, expr, _ = List.nth fields !index in
                          string_of_exp_raw expr)
                         (Types.string_of_ty (List.nth actual_types !index))
                         (Types.string_of_ty (List.nth expected_types !index)))
                      pos;
                    Types.Error)
                  else if expected = Types.Error || actual = Types.Error then
                    Types.Error
                  else (
                    incr index;
                    actual))
                expected_types actual_types
            in
            if
              List.fold_left
                (fun is_error ty ->
                  if is_error || ty = Types.Error then true else false)
                false fields_ty
            then Types.Error
            else record_type
          with
          | Failure _ ->
              print_error Type_error
                (Printf.sprintf
                   "the number of fields for record %s is not correct"
                   (Symbol.name s))
                pos;
              Types.Error
          | Invalid_argument _ ->
              print_error Type_error
                (Printf.sprintf
                   "the number of fields for record %s is not correct"
                   (Symbol.name s))
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
              Types.Error))

and check_seq venv tenv inside_loop exps level =
  let is_break = ref false in
  List.fold_left
    (fun { exp = _; ty } (expr, _) ->
      if ty = Types.Error then { exp = (); ty = Types.Error }
      else
        match expr with
        | Syntax.BreakExp _ ->
            if !is_break then trans_exp (venv, tenv, false, expr, level)
            else (
              is_break := true;
              trans_exp (venv, tenv, inside_loop, expr, level))
        | _ -> trans_exp (venv, tenv, inside_loop, expr, level))
    { exp = (); ty = Types.Unit }
    exps

and check_assign venv tenv inside_loop var exp pos level =
  let { exp = _; ty = var_type } =
    trans_var (venv, tenv, inside_loop, var, level)
  in
  let { exp = _; ty = exp_type } =
    trans_exp (venv, tenv, inside_loop, exp, level)
  in
  if not (Types.check var_type exp_type) then (
    print_error Type_error
      (Printf.sprintf "expression %s has type %s but expected type %s"
         (string_of_exp_raw exp)
         (Types.string_of_ty exp_type)
         (Types.string_of_ty var_type))
      pos;
    Types.Error)
  else if var_type = Types.Error || exp_type = Types.Error then Types.Error
  else Types.Unit

and check_if venv tenv inside_loop test then' else' pos level =
  match else' with
  | None ->
      let { exp = _; ty = test_ty } =
        trans_exp (venv, tenv, inside_loop, test, level)
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
          trans_exp (venv, tenv, inside_loop, then', level)
        in
        if not (Types.check Types.Unit then_ty) then (
          print_error Type_error
            (Printf.sprintf "expression %s has type %s but expected type unit"
               (string_of_exp_raw then')
               (Types.string_of_ty then_ty))
            pos;
          Types.Error)
        else if test_ty = Types.Error || then_ty = Types.Error then Types.Error
        else Types.Unit
  | Some else_exp ->
      let { exp = _; ty = test_ty } =
        trans_exp (venv, tenv, inside_loop, test, level)
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
          trans_exp (venv, tenv, inside_loop, then', level)
        in
        let { exp = _; ty = else_ty } =
          trans_exp (venv, tenv, inside_loop, else_exp, level)
        in
        if not (Types.check_record_nil then_ty else_ty) then (
          print_error Type_error
            (Printf.sprintf "expression %s and %s has different types %s and %s"
               (string_of_exp_raw then')
               (string_of_exp_raw else_exp)
               (Types.string_of_ty then_ty)
               (Types.string_of_ty else_ty))
            pos;
          Types.Error)
        else if
          test_ty = Types.Error || then_ty = Types.Error
          || else_ty = Types.Error
        then Types.Error
        else else_ty

and check_while venv tenv inside_loop test body pos level =
  let { exp = _; ty = test_ty } =
    trans_exp (venv, tenv, inside_loop, test, level)
  in
  if not (Types.check Types.Int test_ty) then (
    print_error Type_error
      (Printf.sprintf "expression %s has type %s but expected type int"
         (string_of_exp_raw test)
         (Types.string_of_ty test_ty))
      pos;
    Types.Error)
  else
    let { exp = _; ty = body_ty } = trans_exp (venv, tenv, true, body, level) in
    if not (Types.check Types.Unit body_ty) then (
      print_error Type_error
        (Printf.sprintf "expression %s has type %s but expected type unit"
           (string_of_exp_raw body)
           (Types.string_of_ty body_ty))
        pos;
      Types.Error)
    else if test_ty = Types.Error || body_ty = Types.Error then Types.Error
    else Types.Unit

and check_for venv tenv inside_loop var escape lo hi body pos level =
  let { exp = _; ty = lo_ty } =
    trans_exp (venv, tenv, inside_loop, lo, level)
  in
  if not (Types.check Types.Int lo_ty) then (
    print_error Type_error
      (Printf.sprintf "expression %s has type %s but expected type int"
         (string_of_exp_raw lo) (Types.string_of_ty lo_ty))
      pos;
    Types.Error)
  else
    let { exp = _; ty = hi_ty } =
      trans_exp (venv, tenv, inside_loop, hi, level)
    in
    if not (Types.check Types.Int hi_ty) then (
      print_error Type_error
        (Printf.sprintf "expression %s has type %s but expected type int"
           (string_of_exp_raw hi) (Types.string_of_ty hi_ty))
        pos;
      Types.Error)
    else
      let { venv = new_venv; tenv = _ }, err =
        trans_dec
          ( venv,
            tenv,
            inside_loop,
            false,
            VarDec { name = var; escape; typ = None; init = lo; pos },
            level )
      in
      let { exp = _; ty = body_ty } =
        trans_exp (new_venv, tenv, true, body, level)
      in
      if not (Types.check Types.Unit body_ty) then (
        print_error Type_error
          (Printf.sprintf "expression %s has type %s but expected type unit"
             (string_of_exp_raw body)
             (Types.string_of_ty body_ty))
          pos;
        Types.Error)
      else if
        err || lo_ty = Types.Error || hi_ty = Types.Error
        || body_ty = Types.Error
      then Types.Error
      else Types.Unit

and check_break inside_loop pos =
  if inside_loop then Types.Unit
  else (
    print_error Name_error "unexpected break" pos;
    Types.Error)

and check_let venv tenv inside_loop decs body level =
  let { venv = new_venv; tenv = new_tenv }, err =
    List.fold_left
      (fun ({ venv = venv'; tenv = tenv' }, dec_error) dec ->
        trans_dec (venv', tenv', inside_loop, dec_error, dec, level))
      ({ venv; tenv }, false)
      decs
  in
  let { exp = _; ty } =
    trans_exp (new_venv, new_tenv, inside_loop, body, level)
  in
  if err then Types.Error else ty

and check_array venv tenv inside_loop typ size init pos level =
  match typ with
  | Syntax.ErrorSym -> Types.Error
  | Syntax.Sym s -> (
      let ty = find_type tenv typ pos in
      match ty with
      | Types.Error -> Types.Error
      | Types.Array (t, u) ->
          let { exp = _; ty = init_ty } =
            trans_exp (venv, tenv, inside_loop, init, level)
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
              trans_exp (venv, tenv, inside_loop, size, level)
            in
            if not (Types.check Types.Int size_ty) then (
              print_error Type_error
                (Printf.sprintf
                   "expression %s has type %s but expected type int"
                   (Syntax.string_of_exp_raw size)
                   (Types.string_of_ty size_ty))
                pos;
              Types.Error)
            else if init_ty = Types.Error || size_ty = Types.Error then
              Types.Error
            else Types.Array (t, u)
      | _ ->
          print_error Type_error
            (Printf.sprintf "name %s has type %s but expected type array"
               (Symbol.name s) (Types.string_of_ty ty))
            pos;
          Types.Error)

and trans_dec (venv, tenv, inside_loop, dec_error, dec, level) =
  match dec with
  | VarDec { name; escape; typ; init; pos } ->
      trans_var_dec venv tenv inside_loop dec_error name escape typ init pos
        level
  | FunctionDec f -> trans_fun_dec venv tenv inside_loop dec_error f level
  | TypeDec t -> trans_type_dec venv tenv dec_error t

and trans_var_dec venv tenv inside_loop dec_error name escape typ init pos level
    =
  match typ with
  | Some (sym, pos) -> (
      match name with
      | Syntax.ErrorSym -> ({ venv; tenv }, true)
      | Syntax.Sym var_name ->
          let expected_ty = find_type tenv sym pos in
          let { exp = _; ty = init_ty } =
            trans_exp (venv, tenv, inside_loop, init, level)
          in
          if not (Types.check_record_nil expected_ty init_ty) then (
            print_error Type_error
              (Printf.sprintf "expression %s has type %s but expected type %s"
                 (Symbol.name var_name)
                 (Types.string_of_ty init_ty)
                 (Types.string_of_ty expected_ty))
              pos;
            ({ venv; tenv }, true))
          else
            let access = Translate.alloc_local level !escape in
            let new_venv =
              Symbol_table.enter var_name
                (Env'.VarEntry { access; ty = expected_ty })
                venv
            in
            if init_ty = Types.Error || expected_ty = Types.Error then
              ({ venv = new_venv; tenv }, true)
            else ({ venv = new_venv; tenv }, dec_error))
  | None -> (
      match name with
      | Syntax.ErrorSym -> ({ venv; tenv }, true)
      | Syntax.Sym var_name ->
          let { exp = _; ty = init_ty } =
            trans_exp (venv, tenv, inside_loop, init, level)
          in
          if Types.check Types.Nil init_ty then (
            print_error Type_error "required type declared variable" pos;
            ({ venv; tenv }, true))
          else
            let access = Translate.alloc_local level !escape in
            let new_venv =
              Symbol_table.enter var_name
                (Env'.VarEntry { access; ty = init_ty })
                venv
            in
            if init_ty = Types.Error then ({ venv = new_venv; tenv }, true)
            else ({ venv = new_venv; tenv }, dec_error))

and find_type tenv typ pos =
  match typ with
  | Syntax.ErrorSym -> Types.Error
  | Syntax.Sym t -> (
      try
        let ty = Symbol_table.look t tenv in
        match ty with
        | Types.Name (s, t) -> (
            match !t with
            | None ->
                print_error Type_error
                  (Printf.sprintf "type %s has an illegal type circulation"
                     (Symbol.name s))
                  pos;
                Types.Error
            | Some ty -> Types.actual_ty ty pos)
        | _ -> Types.actual_ty ty pos
      with Not_found ->
        print_error Name_error
          (Printf.sprintf "type %s is not declared" (Symbol.name t))
          pos;
        Types.Error)

and find_rec_type tenv typ pos =
  match typ with
  | Syntax.ErrorSym -> Types.Error
  | Syntax.Sym t -> (
      try
        let ty = Symbol_table.look t tenv in
        Types.actual_rec_ty ty pos
      with Not_found ->
        print_error Name_error
          (Printf.sprintf "type %s is not declared" (Symbol.name t))
          pos;
        Types.Error)

and enter_field ({ venv; tenv }, dec_error, level, nth)
    { name; escape = _; typ; pos } =
  match name with
  | Syntax.ErrorSym -> ({ venv; tenv }, true, level, nth + 1)
  | Syntax.Sym s -> (
      let ty = find_type tenv typ pos in
      match Translate.look_formal_access level nth with
      | Some access ->
          let new_venv =
            Symbol_table.enter s (Env'.VarEntry { access; ty }) venv
          in
          ({ venv = new_venv; tenv }, dec_error, level, nth + 1)
      | None -> ({ venv; tenv }, dec_error, level, nth + 1))

and trans_fun_dec venv tenv inside_loop dec_error f level =
  let trans_fun_header ({ venv; tenv }, dec_error, fun_names, levels)
      { fname; params; result; body = _; fpos } =
    let param_types =
      List.map
        (fun { name = _; escape = _; typ; pos } -> find_type tenv typ pos)
        params
    in
    let param_escapes = List.map (fun p -> !(p.escape)) params in
    let label = Temp.newlabel () in
    let level' =
      Translate.new_level
        { parent = level; name = label; formals = param_escapes }
    in
    let result_type =
      match result with
      | None -> Types.Unit
      | Some typ -> find_type tenv (fst typ) (snd typ)
    in
    match fname with
    | Syntax.ErrorSym -> ({ venv; tenv }, true, fun_names, level' :: levels)
    | Syntax.Sym name -> (
        try
          let _ = List.find (fun n -> n = name) fun_names in
          print_error Name_error
            (Printf.sprintf "function %s is already declared" (Symbol.name name))
            fpos;
          ({ venv; tenv }, true, fun_names, level' :: levels)
        with Not_found ->
          let new_fun_names = List.append [ name ] fun_names in
          let new_venv =
            Symbol_table.enter name
              (Env'.FunEntry
                 {
                   level = level';
                   label;
                   formals = param_types;
                   result = result_type;
                 })
              venv
          in
          if
            List.fold_left
              (fun is_error ty ->
                if is_error then is_error else ty = Types.Error)
              false param_types
            || result_type = Types.Error
          then ({ venv = new_venv; tenv }, true, new_fun_names, level' :: levels)
          else
            ( { venv = new_venv; tenv },
              dec_error,
              new_fun_names,
              level' :: levels ))
  in
  let { venv = new_venv; tenv = _ }, err, _, levels =
    List.fold_left trans_fun_header ({ venv; tenv }, dec_error, [], []) f
  in
  let fun_types =
    try
      List.map2
        (fun { fname; params; result; body; fpos } lev ->
          let { venv = new_venv'; tenv = _ }, err', _, _ =
            List.fold_left enter_field
              ({ venv = new_venv; tenv }, err, lev, 1)
              params
          in
          let { exp = _; ty = body_ty } =
            let expty' = trans_exp (new_venv', tenv, inside_loop, body, lev) in
            print_endline (Translate.string_of_level lev);
            expty'
          in
          let result_ty =
            match result with
            | None -> Types.Unit
            | Some sym -> find_type tenv (fst sym) (snd sym)
          in
          match fname with
          | Syntax.ErrorSym -> Types.Error
          | Syntax.Sym name ->
              if not (Types.check_record_nil body_ty result_ty) then (
                print_error Type_error
                  (Printf.sprintf
                     "function %s return type %s but expected type %s"
                     (Symbol.name name)
                     (Types.string_of_ty body_ty)
                     (Types.string_of_ty result_ty))
                  fpos;
                Types.Error)
              else if err' || body_ty = Types.Error || result_ty = Types.Error
              then Types.Error
              else result_ty)
        f (List.rev levels)
    with Invalid_argument _ -> [ Types.Error ]
  in
  let is_err =
    List.fold_left
      (fun is_error ty -> if is_error then is_error else ty = Types.Error)
      false fun_types
  in
  ({ venv = new_venv; tenv }, is_err)

and trans_type_header ({ venv; tenv }, dec_error) { tname; ty = _; tpos = _ } =
  match tname with
  | Syntax.ErrorSym -> ({ venv; tenv }, true)
  | Syntax.Sym name ->
      let new_tenv =
        Symbol_table.enter name (Types.Name (name, ref None)) tenv
      in
      ({ venv; tenv = new_tenv }, dec_error)

and trans_type_body tenv dec_error { tname; ty; tpos } =
  match tname with
  | Syntax.ErrorSym -> true
  | Syntax.Sym type_name -> (
      try
        let t = Symbol_table.look type_name tenv in
        match t with
        | Types.Name (sym, typ) -> (
            match !typ with
            | None ->
                let new_ty = trans_ty (tenv, ty) in
                typ := Some new_ty;
                if new_ty = Types.Error then true else dec_error
            | Some _ ->
                print_error Name_error
                  (Printf.sprintf "type %s is already declared"
                     (Symbol.name sym))
                  tpos;
                true)
        | _ ->
            print_error Name_error
              (Printf.sprintf "type %s is already declared"
                 (Symbol.name type_name))
              tpos;
            true
      with Not_found ->
        print_error Name_error
          (Printf.sprintf "type %s is not declared" (Symbol.name type_name))
          tpos;
        true)

and trans_type_dec venv tenv dec_error t =
  let { venv = _; tenv = new_tenv }, err =
    List.fold_left trans_type_header ({ venv; tenv }, dec_error) t
  in
  let types = List.map (trans_type_body new_tenv err) t in
  if
    List.fold_left
      (fun is_error t -> if is_error then is_error else t)
      false types
    || err
  then ({ venv; tenv = new_tenv }, true)
  else ({ venv; tenv = new_tenv }, dec_error)

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
                let field_type = find_rec_type tenv typ pos in
                (field_name, field_type))
          fields
      in
      if
        List.fold_left
          (fun is_error field ->
            if is_error || snd field = Types.Error then true else false)
          false record_fields
      then Types.Error
      else Types.Record (record_fields, ref ())
  | ArrayTy (sym, pos) ->
      let array_type = find_rec_type tenv sym pos in
      if array_type = Types.Error then Types.Error
      else Types.Array (array_type, ref ())
