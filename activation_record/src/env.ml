open Semantic_action
open Semantic_analysis

type enventry =
  | VarEntry of { access : Translate.access; ty : Types.ty }
  | FunEntry of {
      level : Translate.level;
      label : Temp.label;
      formals : Types.ty list;
      result : Types.ty;
    }

let base_tenv =
  Symbol_table.empty
  |> Symbol_table.enter (Symbol.from_string "int") Types.Int
  |> Symbol_table.enter (Symbol.from_string "string") Types.String

let base_venv =
  Symbol_table.empty
  |> Symbol_table.enter
       (Symbol.from_string "print")
       (let lev = Translate.outermost in
        FunEntry
          {
            level = lev;
            label = lev.frame.label;
            formals = [ Types.String ];
            result = Types.Unit;
          })
  |> Symbol_table.enter
       (Symbol.from_string "flush")
       (let lev = Translate.outermost in
        FunEntry
          {
            level = lev;
            label = lev.frame.label;
            formals = [];
            result = Types.Unit;
          })
  |> Symbol_table.enter
       (Symbol.from_string "getchar")
       (let lev = Translate.outermost in
        FunEntry
          {
            level = lev;
            label = lev.frame.label;
            formals = [];
            result = Types.String;
          })
  |> Symbol_table.enter (Symbol.from_string "ord")
       (let lev = Translate.outermost in
        FunEntry
          {
            level = lev;
            label = lev.frame.label;
            formals = [ Types.String ];
            result = Types.Int;
          })
  |> Symbol_table.enter (Symbol.from_string "chr")
       (let lev = Translate.outermost in
        FunEntry
          {
            level = lev;
            label = lev.frame.label;
            formals = [ Types.Int ];
            result = Types.String;
          })
  |> Symbol_table.enter
       (Symbol.from_string "size")
       (let lev = Translate.outermost in
        FunEntry
          {
            level = lev;
            label = lev.frame.label;
            formals = [ Types.String ];
            result = Types.Int;
          })
  |> Symbol_table.enter
       (Symbol.from_string "substring")
       (let lev = Translate.outermost in
        FunEntry
          {
            level = lev;
            label = lev.frame.label;
            formals = [ Types.String; Types.Int; Types.Int ];
            result = Types.String;
          })
  |> Symbol_table.enter
       (Symbol.from_string "concat")
       (let lev = Translate.outermost in
        FunEntry
          {
            level = lev;
            label = lev.frame.label;
            formals = [ Types.String; Types.String ];
            result = Types.String;
          })
  |> Symbol_table.enter (Symbol.from_string "not")
       (let lev = Translate.outermost in
        FunEntry
          {
            level = lev;
            label = lev.frame.label;
            formals = [ Types.Int ];
            result = Types.Int;
          })
  |> Symbol_table.enter
       (Symbol.from_string "exit")
       (let lev = Translate.outermost in
        FunEntry
          {
            level = lev;
            label = lev.frame.label;
            formals = [ Types.Int ];
            result = Types.Unit;
          })
