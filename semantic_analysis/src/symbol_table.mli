open Semantic_action

type 'a table

val empty : 'a table
val enter : Symbol.t -> 'a -> 'a table -> 'a table
val look : Symbol.t -> 'a table -> 'a
