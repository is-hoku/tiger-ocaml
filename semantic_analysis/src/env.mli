type enventry =
  | VarEntry of { ty : Types.ty }
  | FunEntry of { formals : Types.ty list; result : Types.ty }

val base_tenv : Types.ty Symbol_table.table
val base_venv : enventry Symbol_table.table
