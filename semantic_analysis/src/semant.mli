open Semantic_action

(* NOTE: Move to Translate module in chapter 6 later *)
module Translate : sig
  type exp = unit
end

type expty = { exp : Translate.exp; ty : Types.ty }
type venv = Env.enventry Symbol_table.table
type tenv = Types.ty Symbol_table.table
type env = { venv : venv; tenv : tenv }

val trans_var : venv * tenv * Syntax.var -> expty
val trans_exp : venv * tenv * Syntax.exp -> expty
val trans_dec : venv * tenv * Syntax.dec -> env
val trans_ty : tenv * Syntax.ty -> Types.ty
