open Semantic_action

type temp

val newtemp : unit -> temp
val makestring : temp -> string

type label = Symbol.t

val postinc : temp ref -> temp
val newlabel : unit -> label
val namedlabel : string -> label
val string_of_label : label -> string
