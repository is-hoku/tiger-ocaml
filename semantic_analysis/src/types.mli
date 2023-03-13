open Semantic_action

type unique = unit ref

type error_ty =
  | Syntax_error (* Syntax Error is already output by the parser *)
  | Type_error of string
  | Name_error of string

type ty =
  | Record of (Symbol.t * ty) list * unique
  | Nil
  | Int
  | String
  | Array of ty * unique
  | Name of Symbol.t * ty option ref
  | Unit
  | Error of error_ty

val string_of_ty : ty -> string
val actual_ty : ty -> Lexing.position -> ty
