type key = string
type tree = LEAF | TREE of tree * key * tree

let empty = LEAF

let rec insert (key, tree) =
  match tree with
  | LEAF -> TREE (LEAF, key, LEAF)
  | TREE (l, k, r) ->
      if key < k then TREE (insert (key, l), k, r)
      else if key > k then TREE (l, k, insert (key, r))
      else TREE (l, key, r)

let rec member (key, tree) =
  match tree with
  | LEAF -> false
  | TREE (l, k, r) ->
      if k < key then member (key, r)
      else if k > key then member (key, l)
      else true
