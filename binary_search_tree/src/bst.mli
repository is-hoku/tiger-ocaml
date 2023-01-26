type key = string
type 'a tree = LEAF | TREE of 'a tree * key * 'a * 'a tree

val empty : 'a tree
val insert : 'a tree * key * 'a -> 'a tree
val member : 'a tree * key -> bool
val lookup : 'a tree * key -> 'a
val string_of_int_tree : int tree -> string
