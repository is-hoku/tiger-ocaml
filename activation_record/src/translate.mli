type level = { parent : level option; frame : Mips_frame.frame }
type access = level * Mips_frame.access

val string_of_level : level -> string
val outermost : level

type level_input = { parent : level; name : Temp.label; formals : bool list }

val new_level : level_input -> level
val formals : level -> access list
val alloc_local : level -> bool -> access
val look_formal_access : level -> int -> access option

(* NOTE: Fix this in chapter7 *)
type exp = unit
