type t = string * int

let from_string (name : string) =
  let hashtable = Hashtbl.create 128 in
  let index = ref (-1) in
  try
    let i = Hashtbl.find hashtable name in
    (name, i)
  with Not_found ->
    incr index;
    Hashtbl.add hashtable name !index;
    (name, !index)

let name = fst
let compare (_, a) (_, b) = Stdlib.compare a b
