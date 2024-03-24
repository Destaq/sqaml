type value =
  | Int of int
  | Varchar of string
  | Float of float
  | Date of string
  | Null

type row = {
  values: value list;
}

val print_row : row -> unit