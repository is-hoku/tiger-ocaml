open Semantic_action

module Int_binary_tree = Map.Make (struct
  type t = Symbol.t

  let compare a b = Symbol.compare a b
end)

type 'a table = 'a Int_binary_tree.t

let empty = Int_binary_tree.empty
let enter sym value table = Int_binary_tree.add sym value table
let look sym table = Int_binary_tree.find sym table
