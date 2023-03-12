open Semantic_action

type enventry =
  | VarEntry of { ty : Types.ty }
  | FunEntry of { formals : Types.ty list; result : Types.ty }

let base_tenv =
  Symbol_table.empty
  |> Symbol_table.enter (Symbol.from_string "int") Types.Int
  |> Symbol_table.enter (Symbol.from_string "string") Types.String

let base_venv =
  Symbol_table.empty
  |> Symbol_table.enter
       (Symbol.from_string "print")
       (FunEntry { formals = [ Types.String ]; result = Types.Unit })
  |> Symbol_table.enter
       (Symbol.from_string "flush")
       (FunEntry { formals = []; result = Types.Unit })
  |> Symbol_table.enter
       (Symbol.from_string "getchar")
       (FunEntry { formals = []; result = Types.String })
  |> Symbol_table.enter (Symbol.from_string "ord")
       (FunEntry { formals = [ Types.String ]; result = Types.Int })
  |> Symbol_table.enter (Symbol.from_string "chr")
       (FunEntry { formals = [ Types.Int ]; result = Types.String })
  |> Symbol_table.enter
       (Symbol.from_string "size")
       (FunEntry { formals = [ Types.String ]; result = Types.Int })
  |> Symbol_table.enter
       (Symbol.from_string "substring")
       (FunEntry
          {
            formals = [ Types.String; Types.Int; Types.Int ];
            result = Types.String;
          })
  |> Symbol_table.enter
       (Symbol.from_string "concat")
       (FunEntry
          { formals = [ Types.String; Types.String ]; result = Types.String })
  |> Symbol_table.enter (Symbol.from_string "not")
       (FunEntry { formals = [ Types.Int ]; result = Types.Int })
  |> Symbol_table.enter
       (Symbol.from_string "exit")
       (FunEntry { formals = [ Types.Int ]; result = Types.Unit })
