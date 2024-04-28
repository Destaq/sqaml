(** The type of a value in a row. *)
type value =
  | Int of int
  | Varchar of string
  | Float of float
  | Date of string
  | Null

type row = { values : value list }
(** The type of a row in the database. *)

val print_value : value -> unit
(** [print_value v] prints the value [v]. *)

val print_row : row -> unit
(** [print_row r] prints the row [r].*)

val value_equals : value -> value -> bool
(** [value_equals v1 v2] returns true if [v1] is structurally equivalent to [v2] and false otherwise. *)

val value_not_equals : value -> value -> bool
(** [value_not_equals v1 v2] returns true if [v1] is not structurally equivalent to [v2] and false otherwise. *)

val value_greater_than : value -> value -> bool
(** [value_greater_than v1 v2] returns true if [v1] is greater than [v2] and false otherwise. *)

val value_less_than : value -> value -> bool
(** [value_less_than v1 v2] returns true if [v1] is less than [v2] and false otherwise. *)
