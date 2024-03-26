(**Type for storing type of token in query.*)
type token =
  | Identifier of string
  | IntKeyword
  | VarcharKeyword
  | PrimaryKey

val parse_and_execute_query : string -> unit

val print_tokenized : token list -> unit

val tokenize_query : string -> token list