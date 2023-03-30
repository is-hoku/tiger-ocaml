open Semantic_action

type temp = int

let temps = ref 100

let newtemp () =
  let t = !temps in
  temps := t + 1;
  t

let makestring t = "t" ^ string_of_int t

type label = Symbol.t

let postinc x =
  let i = !x in
  x := i + 1;
  i

(* let labs = ref 0 *)

let newlabel () =
  incr temps;
  Symbol.from_string (string_of_int !temps)

let namedlabel (name : string) = (name, newtemp ())
let string_of_label l = Symbol.name l
