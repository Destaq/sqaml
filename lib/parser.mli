(** Type for storing type of token in query. *)
type token = Identifier of string | IntKeyword | VarcharKeyword | PrimaryKey

val parse_and_execute_query : string -> unit
(** [parse_and_execute_query q] executes the query denoted by [q]. *)

val print_tokenized : token list -> unit
(** [print_tokenized q] prints the tokenization of query [q]. *)

val tokenize_query : string -> token list
(** [tokenize_query q] returns the tokenization of query [q]. *)
