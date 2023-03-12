type t = string * int

val from_string : string -> t
val name : t -> string
val compare : 'a * 'b -> 'c * 'b -> int
