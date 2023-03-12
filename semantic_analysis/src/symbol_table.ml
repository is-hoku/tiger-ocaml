open Semantic_action

module IntBinaryTree = Map.Make (struct
  type t = Symbol.t

  let compare a b = Symbol.compare a b
end)

type 'a table = 'a IntBinaryTree.t

let empty = IntBinaryTree.empty
let enter sym value table = IntBinaryTree.add sym value table
let look table sym = IntBinaryTree.find table sym
