type access = InFrame of int | InReg of Temp.temp

type frame = {
  index : int;
  label : Temp.label;
  formals : access list;
  mutable locals : int;
}

let string_of_access = function
  | InFrame i -> "f" ^ string_of_int i
  | InReg t -> Temp.makestring t

type frame_input = { name : Temp.label; params : bool list }

let idx = ref (-1)

let next_index () =
  incr idx;
  !idx

let make_access n b = if b then InFrame n else InReg (Temp.newtemp ())

let new_frame { name; params } =
  {
    index = next_index ();
    label = name;
    formals = List.mapi make_access params;
    locals = 0;
  }

let name f = f.label
let formals f = f.formals

let alloc_local f b =
  if b then f.locals <- f.locals + 1;
  make_access f.locals b
