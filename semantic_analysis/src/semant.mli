open Semantic_action

(* NOTE: Move to Translate module in chapter 6 *)
module Translate : sig
  type exp = unit
end

type expty = { exp : Translate.exp; ty : Types.ty }
type venv = Env.enventry Symbol_table.table
type tenv = Types.ty Symbol_table.table
type env = { venv : venv; tenv : tenv }
type inside_loop = bool
type dec_error = bool

val trans_var : venv * tenv * inside_loop * Syntax.var -> expty
val trans_exp : venv * tenv * inside_loop * Syntax.exp -> expty

val trans_dec :
  venv * tenv * inside_loop * dec_error * Syntax.dec -> env * dec_error

val trans_ty : tenv * Syntax.ty -> Types.ty
