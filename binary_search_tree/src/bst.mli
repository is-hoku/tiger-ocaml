type key = string
type tree = LEAF | TREE of tree * key * tree

val empty : tree
val insert : key * tree -> tree
val member : key * tree -> bool
