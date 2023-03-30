open Semantic_analysis

type enventry =
  | VarEntry of { access : Translate.access; ty : Types.ty }
  | FunEntry of {
      level : Translate.level;
      label : Temp.label;
      formals : Types.ty list;
      result : Types.ty;
    }

val base_tenv : Types.ty Symbol_table.table
val base_venv : enventry Symbol_table.table
