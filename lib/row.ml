type value = 
  | Int of int
  | Varchar of string
  | Float of float
  | Date of string
  | Null

type row = {
  values: value list;
}

let print_row row = 
  List.iter (fun v -> match v with
      | Int i -> Printf.printf "%d " i
      | Varchar s -> Printf.printf "%s " s
      | Float f -> Printf.printf "%f " f
      | Date d -> Printf.printf "%s " d
      | Null -> Printf.printf "NULL ") row.values;
  Printf.printf "\n"




