open Semantic_action
open Error

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

let rec string_of_ty = function
  | Record (fields, _) ->
      String.concat ","
        (List.map
           (fun (s, t) ->
             Printf.sprintf "%s:%s" (Symbol.name s) (string_of_ty t))
           fields)
  | Nil -> "nil"
  | Int -> "int"
  | String -> "string"
  | Array (t, _) -> Printf.sprintf "array of %s" (string_of_ty t)
  | Name (s, t) -> (
      match !t with
      | Some typ ->
          Printf.sprintf "name of %s(%s)" (Symbol.name s) (string_of_ty typ)
      | None -> "name of %s(?)")
  | Unit -> "()"
  | Error _ -> "Error"

let rec actual_ty t (pos : Lexing.position) =
  match t with
  | Record (fields, u) -> Record (fields, u)
  | Nil -> Nil
  | Int -> Int
  | String -> String
  | Array (t, u) -> Array (t, u)
  | Name (s, t) -> (
      match !t with
      | Some typ -> actual_ty typ pos
      | None ->
          raise
            (Type_error
               (Printf.sprintf
                  "type %s is not initialized in line %d at character %d"
                  (Symbol.name s) pos.pos_lnum
                  (pos.pos_cnum - pos.pos_bol))))
  | Unit -> Unit
  | Error s -> Error s
