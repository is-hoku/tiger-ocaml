type access = InFrame of int | InReg of Temp.temp

type frame = {
  index : int;
  label : Temp.label;
  formals : access list;
  mutable locals : int;
}

val string_of_access : access -> string

type frame_input = { name : Temp.label; params : bool list }

val new_frame : frame_input -> frame
val name : frame -> Temp.label
val formals : frame -> access list
val alloc_local : frame -> bool -> access
