type t = string * int

val from_string : string -> t
val name : t -> string
val compare : 'a * int -> 'b * int -> int
val dummy : string * int
