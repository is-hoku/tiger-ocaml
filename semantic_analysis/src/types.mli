open Semantic_action

type unique = unit ref

type ty =
  | Record of (Symbol.t * ty) list * unique
  | Nil
  | Int
  | String
  | Array of ty * unique
  | Name of Symbol.t * ty option ref
  | Unit
  | Error

val string_of_ty : ty -> string
val actual_ty : ty -> Lexing.position -> ty
val check : ty -> ty -> bool
val check_record_nil : ty -> ty -> bool
