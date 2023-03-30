type level = { parent : level option; frame : Mips_frame.frame }
type access = level * Mips_frame.access

let string_of_level lev =
  let f = lev.frame in
  Printf.sprintf "{index(%d), label(%s), formals(%s), locals(%s)}" f.index
    (Temp.string_of_label f.label)
    (String.concat ", " (List.map Mips_frame.string_of_access f.formals))
    (string_of_int f.locals)

let outermost =
  let label = Temp.newlabel () in
  let frame = Mips_frame.new_frame { name = label; params = [] } in
  { parent = None; frame }

type level_input = { parent : level; name : Temp.label; formals : bool list }

let new_level { parent; name; formals } =
  let frame = Mips_frame.new_frame { name; params = true :: formals } in
  { parent = Some parent; frame }

let formals lev = List.map (fun access -> (lev, access)) lev.frame.formals
let alloc_local lev b = (lev, Mips_frame.alloc_local lev.frame b)

let look_formal_access lev n =
  try
    let access = List.nth lev.frame.formals n in
    Some (lev, access)
  with Invalid_argument _ -> None

(* NOTE: Fix this in chapter7 *)
type exp = unit
