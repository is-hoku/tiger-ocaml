open Lexical_analysis.Token

val parsertoken_of_tokent : t -> Parser.token
val to_string : Parser.token -> string
