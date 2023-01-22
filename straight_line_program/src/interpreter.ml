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
  | CompoundStm (s1, s2) -> max (maxargs s1, maxargs s2)
  | AssignStm (_, s1) -> maxargs_exp s1
  | PrintStm el -> max (List.length el, max_list (List.map maxargs_exp el))

and maxargs_exp e =
  match e with
  | IdExp _ -> 0
  | NumExp _ -> 0
  | OpExp (e1, _, e2) -> max (maxargs_exp e1, maxargs_exp e2)
  | EseqExp (s1, e1) -> max (maxargs s1, maxargs_exp e1)

let calcurate (a, op, b) =
  match op with Plus -> a + b | Minus -> a - b | Times -> a * b | Div -> a / b

let rec print_table t =
  match t with
  | [] -> ()
  | (id1, v) :: tl ->
      print_string (id1 ^ ":");
      print_int v;
      print_char ' ';
      print_table tl

let rec lookup (id1, t) =
  match t with
  | [] -> failwith "lookup called on empty list"
  | (id0, v) :: tl -> if id0 = id1 then v else lookup (id1, tl)

let rec interpStm (s, t) =
  match s with
  | CompoundStm (s1, s2) ->
      let t1 = interpStm (s1, t) in
      interpStm (s2, t1)
  | AssignStm (id1, e1) ->
      let v, t1 = interpExp (e1, t) in
      (id1, v) :: t1
  | PrintStm el ->
      let t1 = print_exp_list (el, t) in
      print_newline ();
      t1

and interpExp (e, t) =
  match e with
  | IdExp id1 -> (lookup (id1, t), t)
  | NumExp e1 -> (e1, t)
  | OpExp (e1, op, e2) ->
      let v1, t1 = interpExp (e1, t) in
      let v2, t2 = interpExp (e2, t1) in
      (calcurate (v1, op, v2), t2)
  | EseqExp (s1, e1) ->
      let t1 = interpStm (s1, t) in
      interpExp (e1, t1)

and print_exp_list (el, t) =
  match el with
  | [] -> t
  | e :: etl ->
      let v, t1 = interpExp (e, t) in
      print_int v;
      print_char ' ';
      print_exp_list (etl, t1)

let interp p = print_table (interpStm (p, []))
