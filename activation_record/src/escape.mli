open Semantic_action
open Semantic_analysis

val find_escape : Syntax.exp -> unit

type depth = int
type esc_env = (depth * bool ref) Symbol_table.table

val traverse_var : esc_env * depth * Syntax.var -> unit
val traverse_exp : esc_env * depth * Syntax.exp -> unit
val traverse_decs : esc_env * depth * Syntax.dec list -> esc_env
