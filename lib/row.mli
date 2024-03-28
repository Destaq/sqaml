type value =
  | Int of int
  | Varchar of string
  | Float of float
  | Date of string
  | Null

type row = { values : value list }

val print_value : value -> unit
val print_row : row -> unit
val value_equals : value -> value -> bool
val value_not_equals : value -> value -> bool
val value_greater_than : value -> value -> bool
val value_less_than : value -> value -> bool
