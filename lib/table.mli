open Row

(** Different types of columns. *)
type column_type =
  | Int_type
  | Varchar_type
  | Float_type
  | Date_type
  | Null_type

type column = { name : string; col_type : column_type; primary_key : bool }
(** Representation type of column. *)

type table
(** Abstracted table type. *)

val construct_transform : string list -> value list -> table -> row -> row

val construct_predicate :
  string list ->
  value list ->
  (value -> value -> bool) list ->
  table ->
  row ->
  bool

val get_columns_lst : table -> bool -> string list
val construct_row_map : table -> row -> (string, value) Hashtbl.t
val convert_to_value : column_type -> string -> value
val get_column_type : table -> string -> column_type
val compare_row : int -> row -> row -> int

val get_table_pk_field : table -> column option
(**[get_pk_field] returns the primary key field in a table.*)

val check_for_pk_value : table -> string -> value -> unit
(**[check_for_pk_value] checks for uniqueness of primary key in a table.*)

val create_table : column list -> table
(** [create_table cl] creates a new table with the columns in [cl]. *)

val insert_row : table -> string list -> string list -> unit
(** [insert_row t n v] inserts a row with column names [n] and values [v] into the table [t]. *)

val update_rows : table -> (row -> bool) -> (row -> row) -> unit
(** Update rows based on a predicate and a transformation function. *)

val delete_rows : table -> (row -> bool) -> unit
(** Delete rows based on a predicate. *)

val select_rows_table :
  table -> string list -> (row -> bool) -> string -> int option * row list
(** Select rows based on a predicate. *)

val print_table : table -> unit
(** Print the table. *)

val select_all : table -> row list
(** Select all rows in the table. *)
