type id = string
type binop = Plus | Minus | Times | Div

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

let max (a, b) = if a > b then a else b

let rec max_list a =
  match a with
  | [] -> failwith "max_list called on empty list"
  | [ x ] -> x
  | x :: xs -> max (x, max_list xs)

let rec maxargs s =
  match s with
  | CompoundStm (a, b) -> max (maxargs a, maxargs b)
  | AssignStm (_, a) -> maxargs_exp a
  | PrintStm a -> max (List.length a, max_list (List.map maxargs_exp a))

and maxargs_exp e =
  match e with
  | IdExp _ -> 0
  | NumExp _ -> 0
  | OpExp (a, _, b) -> max (maxargs_exp a, maxargs_exp b)
  | EseqExp (a, b) -> max (maxargs a, maxargs_exp b)
