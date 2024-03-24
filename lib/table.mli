open Row

type column_type =
  | Int_type
  | Varchar_type
  | Float_type
  | Date_type
  | Null_type

type column = {
name: string;
col_type: column_type;
primary_key: bool;
}
type table

val create_table : column list -> table
(** Create a new table with the given columns. *)

val insert_row : table -> string list ->  string list -> unit
(** Insert a row into the table. *)

val update_rows : table -> (row -> bool) -> (row -> row) -> unit
(** Update rows based on a predicate and a transformation function. *)

val delete_rows : table -> (row -> bool) -> unit
(** Delete rows based on a predicate. *)

val select_rows : table -> string list -> (row -> bool) -> row list
(** Select rows based on a predicate. *)

val print_table : table -> unit
(** Print the table. *)

val select_all : table -> row list
(** Select all rows in the table. *)
