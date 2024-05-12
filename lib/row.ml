type value =
  | Int of int
  | Varchar of string
  | Float of float
  | Date of string
  | Null

type row = { values : value list }

let print_value v =
  match v with
  | Int i -> print_int i
  | Varchar s -> print_string s
  | Float f -> print_float f
  | Date d -> print_string d
  | Null -> print_string "null"

let value_equals val1 val2 =
  match (val1, val2) with
  | Int v1, Int v2 -> v1 = v2
  | Varchar v1, Varchar v2 -> String.compare v1 v2 = 0
  | Float v1, Float v2 -> v1 = v2
  | Date v1, Date v2 -> String.compare v1 v2 = 0
  | _ -> false

let value_not_equals val1 val2 = not (value_equals val1 val2)

let convert_to_string = function
  | Int x -> string_of_int x
  | Varchar x -> x
  | _ -> failwith "Bad type."

let rec convert_values = function
  | [] -> []
  | h :: t -> convert_to_string h :: convert_values t

let to_list r = convert_values r.values

(**Requires YYYY-MM-DD format.*)
let value_greater_than val1 val2 =
  match (val1, val2) with
  | Int v1, Int v2 -> v1 > v2
  | Varchar v1, Varchar v2 -> String.compare v1 v2 > 0
  | Float v1, Float v2 -> v1 > v2
  | Date v1, Date v2 -> String.compare v1 v2 > 0
  | _ -> false

let value_less_than val1 val2 =
  match (val1, val2) with
  | Int v1, Int v2 -> v1 < v2
  | Varchar v1, Varchar v2 -> String.compare v1 v2 < 0
  | Float v1, Float v2 -> v1 < v2
  | Date v1, Date v2 -> String.compare v1 v2 < 0
  | _ -> false

let print_row row =
  List.iter
    (fun v ->
      match v with
      | Int i -> Printf.printf "%d " i
      | Varchar s -> Printf.printf "%s " s
      | Float f -> Printf.printf "%f " f
      | Date d -> Printf.printf "%s " d
      | Null -> Printf.printf "NULL ")
    row.values;
  Printf.printf "\n"
