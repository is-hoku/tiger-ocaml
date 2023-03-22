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
      Printf.sprintf "{%s}"
        (String.concat ","
           (List.map
              (fun (s, t) ->
                Printf.sprintf "%s:%s" (Symbol.name s) (string_of_rec_ty t))
              fields))
  | Nil -> "nil"
  | Int -> "int"
  | String -> "string"
  | Array (t, _) -> Printf.sprintf "array of %s" (string_of_rec_ty t)
  | Name (s, t) -> (
      match !t with
      | Some typ ->
          Printf.sprintf "name of %s(%s)" (Symbol.name s) (string_of_ty typ)
      | None -> Printf.sprintf "name of %s(?)" (Symbol.name s))
  | Unit -> "unit"
  | Error -> "error"

and string_of_rec_ty = function
  | Record (fields, _) ->
      Printf.sprintf "{%s}"
        (String.concat ","
           (List.map
              (fun (s, t) ->
                Printf.sprintf "%s:%s" (Symbol.name s) (string_of_rec_ty t))
              fields))
  | Nil -> "nil"
  | Int -> "int"
  | String -> "string"
  | Array (t, _) -> Printf.sprintf "array of %s" (string_of_rec_ty t)
  | Name (s, t) -> (
      match !t with
      | Some _ -> Printf.sprintf "name of %s(rec)" (Symbol.name s)
      | None -> Printf.sprintf "name of %s(?)" (Symbol.name s))
  | Unit -> "unit"
  | Error -> "error"

let rec actual_ty t (pos : Lexing.position) =
  match t with
  | Record (fields, u) ->
      Record
        (List.map (fun (name, typ) -> (name, actual_rec_ty typ pos)) fields, u)
  | Nil -> Nil
  | Int -> Int
  | String -> String
  | Array (t, u) -> Array (actual_rec_ty t pos, u)
  | Name (s, ty) -> (
      match !ty with
      | Some s -> actual_ty s pos
      | None ->
          print_error Type_error
            (Printf.sprintf "type %s is not initialized" (Symbol.name s))
            pos;
          raise Type_error)
  | Unit -> Unit
  | Error -> Error

and actual_rec_ty t (pos : Lexing.position) =
  match t with
  | Record (fields, u) ->
      Record
        (List.map (fun (name, typ) -> (name, actual_rec_ty typ pos)) fields, u)
  | Nil -> Nil
  | Int -> Int
  | String -> String
  | Array (t, u) -> Array (actual_rec_ty t pos, u)
  | Name (s, ty) -> Name (s, ty)
  | Unit -> Unit
  | Error -> Error

let rec check expected actual =
  match (expected, actual) with
  | Record _, Nil -> true
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

let check_record_nil expected actual =
  match (expected, actual) with
  | Record _, Nil | Nil, Record _ -> true
  | _ -> check expected actual
