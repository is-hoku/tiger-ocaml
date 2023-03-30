open Semantic_action
open Semantic_action.Syntax
open Semantic_analysis

type depth = int
type esc_env = (depth * bool ref) Symbol_table.table

let rec traverse_var (env, d, s) =
  match s with
  | SimpleVar (sym, _) -> (
      match sym with
      | Syntax.ErrorSym -> ()
      | Syntax.Sym var_name -> (
          try
            let dep, esc = Symbol_table.look var_name env in
            if d > dep then esc := true
          with Not_found -> ()))
  | FieldVar (var, sym, _) -> (
      traverse_var (env, d, var);
      match sym with
      | Syntax.ErrorSym -> ()
      | Syntax.Sym s ->
          let dep, esc = Symbol_table.look s env in
          if d > dep then esc := true)
  | SubscriptVar (var, exp, _) ->
      traverse_var (env, d, var);
      traverse_exp (env, d, exp)

and traverse_exp (env, d, s) =
  match s with
  | VarExp var -> traverse_var (env, d, var)
  | NilExp | IntExp _ | StringExp (_, _) | BreakExp _ -> ()
  | CallExp { func = _; args; pos = _ } ->
      List.iter (fun expr -> traverse_exp (env, d, expr)) args
  | OpExp { left; oper = _; right; _ } ->
      traverse_exp (env, d, left);
      traverse_exp (env, d, right)
  | RecordExp { fields; _ } ->
      List.iter
        (fun expr -> traverse_exp (env, d, expr))
        (List.map (fun (_, e, _) -> e) fields)
  | SeqExp exps ->
      List.iter (fun expr -> traverse_exp (env, d, expr)) (List.map fst exps)
  | AssignExp { var; exp; _ } ->
      traverse_var (env, d, var);
      traverse_exp (env, d, exp)
  | IfExp { test; then'; else'; _ } -> (
      traverse_exp (env, d, test);
      traverse_exp (env, d, then');
      match else' with Some e -> traverse_exp (env, d, e) | None -> ())
  | WhileExp { test; body; _ } ->
      traverse_exp (env, d, test);
      traverse_exp (env, d, body)
  | ForExp { var; escape; lo; hi; body; _ } -> (
      match var with
      | ErrorSym -> ()
      | Sym s ->
          let new_env = traverse_var_dec s env d lo escape in
          traverse_exp (new_env, d, hi);
          traverse_exp (new_env, d, body))
  | LetExp { decs; body; _ } ->
      let new_env = traverse_decs (env, d, decs) in
      traverse_exp (new_env, d, body)
  | ArrayExp { typ = _; size; init; _ } ->
      traverse_exp (env, d, size);
      traverse_exp (env, d, init)
  | ErrorExp -> ()

and traverse_decs (env, d, decs) =
  List.fold_left
    (fun e dec ->
      match dec with
      | VarDec { name; escape; typ = _; init; _ } -> (
          match name with
          | Syntax.ErrorSym -> env
          | Syntax.Sym s -> traverse_var_dec s e d init escape)
      | FunctionDec f -> traverse_fun_decs e d f
      | TypeDec types -> traverse_ty_decs e d types)
    env decs

and traverse_var_dec name env d init esc =
  traverse_exp (env, d, init);
  Symbol_table.enter name (d, esc) env

and traverse_fun_decs e d funs =
  let d' = d + 1 in
  List.fold_left (fun env f -> traverse_fun_dec env d' f) e funs

and traverse_fun_dec e d { fname = _; params; result = _; body; _ } =
  let new_env =
    let param_count = ref 1 in
    List.fold_left
      (fun env { name; escape; _ } ->
        match name with
        | Syntax.ErrorSym -> env
        | Syntax.Sym s ->
            let esc =
              if !param_count > 4 then (
                escape := true;
                escape)
              else escape
            in
            incr param_count;
            Symbol_table.enter s (d, esc) env)
      e params
  in
  traverse_exp (new_env, d, body);
  new_env

and traverse_ty_decs e d types =
  List.fold_left (fun env t -> traverse_ty_dec env d t) e types

and traverse_ty_dec e d { tname = _; ty; _ } =
  match ty with
  | NameTy (_, _) | ArrayTy (_, _) -> e
  | RecordTy fields ->
      List.fold_left
        (fun env { name; escape; _ } ->
          match name with
          | Syntax.ErrorSym -> env
          | Syntax.Sym s -> Symbol_table.enter s (d, escape) env)
        e fields

let find_escape ast = traverse_exp (Symbol_table.empty, 0, ast)
