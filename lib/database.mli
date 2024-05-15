(* database.mli *)

open Table
open Row

val construct_transform : string list -> value list -> string -> row -> row
(** [construct_transform] constructs a transformation function from a set clause. *)

val construct_predicate :
  string list ->
  value list ->
  (value -> value -> bool) list ->
  string ->
  row ->
  bool
(** [construct_predicate] constructs a predicate from a where clause. *)

val tables : (string, table ref) Hashtbl.t
(** Main tables variable to store the database tables.*)

val get_pk_field : string -> column option
(**[get_pk_field] returns the primary key field in a table.*)

val check_pk_uniqueness : string -> string -> value -> unit
(**[check_pk_uniqueness] throws an exception if a primary key is not unique.*)

val get_table_columns : string -> bool -> string list
(** [get_table_columns] returns the full list of columns in a table. *)

val get_column_type : string -> string -> column_type
(** [get_column_type t c] gets the type of column [c] of a table [t] in the database. *)

val show_all_tables : unit -> unit
(** [show_all_tables] prints the list of tables currently in the database. *)

val drop_table : string -> unit
(** [drop_table] drops a table from the database, by name.*)

val create_table : column list -> string -> unit
(** [create_table columns] creates a new table with the given columns. *)

val insert_row : string -> string list -> string list -> unit
(** [insert_row table row] inserts a row into the table. *)

val update_rows : string -> (row -> bool) -> (row -> row) -> unit
(** [update_rows table predicate transform] updates rows based on a predicate and a transformation function. *)

val delete_rows : string -> (row -> bool) -> unit
(** [delete_rows table predicate] deletes rows based on a predicate. *)

val select_rows :
  string -> string list -> (row -> bool) -> string -> int option * row list
(** [select_rows table fields predicate] selects rows based on a predicate. *)

val print_table : string -> unit
(** [print_table table] prints the table. *)

val select_all : string -> unit
(** [select_all] selects every row and column from the table*)

val sorter : string -> int -> row -> row -> int
(**[sorter] constructs a sorting function given table name, column index, and 2 rows.*)
