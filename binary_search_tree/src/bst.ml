type key = string
type 'a tree = LEAF | TREE of 'a tree * key * 'a * 'a tree

let empty = LEAF

let rec insert (tree, key, value) =
  match tree with
  | LEAF -> TREE (LEAF, key, value, LEAF)
  | TREE (l, k, v, r) ->
      if key < k then TREE (insert (l, key, value), k, v, r)
      else if key > k then TREE (l, k, v, insert (r, key, value))
      else TREE (l, key, value, r)

let rec member (tree, key) =
  match tree with
  | LEAF -> false
  | TREE (l, k, _, r) ->
      if k < key then member (r, key)
      else if k > key then member (l, key)
      else true

let rec lookup (tree, key) =
  match tree with
  | LEAF -> failwith "not found"
  | TREE (l, k, v, r) ->
      if k < key then lookup (r, key)
      else if k > key then lookup (l, key)
      else v

let rec string_of_int_tree t =
  match t with
  | LEAF -> "Leaf"
  | TREE (l, k, v, r) ->
      "Tree(" ^ string_of_int_tree l ^ "," ^ string_of_int v ^ "," ^ k ^ ","
      ^ string_of_int_tree r ^ ")"
