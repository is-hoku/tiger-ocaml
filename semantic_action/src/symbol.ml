type t = string * int

let index = ref (-1)
let hashtable = Hashtbl.create 128

let from_string (name : string) =
  try
    let i = Hashtbl.find hashtable name in
    (name, i)
  with Not_found ->
    incr index;
    Hashtbl.add hashtable name !index;
    (name, !index)

let name = fst
let compare (_, a) (_, b) = Int.compare a b
let dummy = ("", 0)
