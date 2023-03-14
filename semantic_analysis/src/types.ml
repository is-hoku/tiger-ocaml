open Semantic_action
open Error

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
  | Error -> "error"

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
  | Error -> Error

let rec check expected actual =
  match (expected, actual) with
  | Record (_, u1), Record (_, u2) -> u1 == u2
  | Nil, Nil -> true
  | Int, Int -> true
  | String, String -> true
  | Array (_, u1), Array (_, u2) -> u1 == u2
  | Name (_, ty), other | other, Name (_, ty) -> (
      match !ty with None -> false | Some t -> check t other)
  | Unit, Unit -> true
  | Error, _ -> true
  | _, Error -> true
  | _ -> false
