(* database.mli *)

open Table
open Row

(** [create_table columns] creates a new table with the given columns. *)
val create_table : column list -> string -> unit

(** [insert_row table row] inserts a row into the table. *)
val insert_row : string -> string list -> string list -> unit

(** [update_rows table predicate transform] updates rows based on a predicate and a transformation function. *)
val update_rows : string -> (row -> bool) -> (row -> row) -> unit

(** [delete_rows table predicate] deletes rows based on a predicate. *)
val delete_rows : string -> (row -> bool) -> unit

(** [select_rows table fields predicate] selects rows based on a predicate. *)
val select_rows : string -> string list -> (row -> bool) -> row list

(** [print_table table] prints the table. *)
val print_table : string -> unit

(** [select_all] selects every row and column from the table*)
val select_all : string -> unit
