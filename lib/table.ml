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

type table = {
  columns: column list;
  mutable rows: row list;
}

let create_table columns =
  let has_primary_key = List.exists (fun col -> col.primary_key) columns in
  if not has_primary_key then
    failwith "Table must have a primary key"
  else
    { columns; rows = [] }

let convert_to_value col_type str =
  match col_type with
  | Int_type -> Int (int_of_string str)
  | Varchar_type -> Varchar str
  | Float_type -> Float (float_of_string str)
  | Date_type -> Date str
  | Null_type -> Null

let insert_row table column_names values =
  if List.length column_names <> List.length values then
    failwith "Number of columns does not match number of values";

  let row_values =
    List.map2 (fun col_name value ->
      let column = List.find (fun col -> col.name = col_name) table.columns in
      match String.trim value with
      | "" -> Null
      | v -> convert_to_value column.col_type v
    ) column_names values
  in

  let new_row = row_values in
  table.rows <- {values = new_row} :: table.rows


let update_rows table pred f =
  table.rows <- List.map (fun r -> if pred r then f r else r) table.rows

let delete_rows table pred =
  table.rows <- List.filter (fun r -> not (pred r)) table.rows

let select_rows table column_names pred =
  let columns =
    List.map (fun name ->
        match List.find_opt (fun c -> c.name = name) table.columns with
        | Some c -> c
        | None -> failwith "Column does not exist"
      ) column_names in
  let filter_row row =
    let filtered_values =
      List.combine table.columns row.values
      |> List.filter (fun (name, _) -> List.mem name columns)
      |> List.map snd
    in
    { values = filtered_values }
  in
  List.filter pred (List.map filter_row table.rows)

let select_all table = table.rows

let print_table table = 
  let print_column column =
    match column.col_type with
    | Int_type -> Printf.printf "%s: int\n" column.name
    | Varchar_type -> Printf.printf "%s: varchar\n" column.name
    | Float_type -> Printf.printf "%s: float\n" column.name
    | Date_type -> Printf.printf "%s: date\n" column.name
    | Null_type -> Printf.printf "%s: null\n" column.name
  in
  List.iter print_column table.columns;
  List.iter (fun row -> print_row row) table.rows
