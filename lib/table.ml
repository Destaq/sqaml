open Row

type column_type =
  | Int_type
  | Varchar_type
  | Float_type
  | Date_type
  | Null_type

type column = { name : string; col_type : column_type; primary_key : bool }
type table = { columns : column list; mutable rows : row list }

(**Get type of a column*)
let get_column_type table col_name =
  let rec get_column_type_aux columns name =
    match columns with
    | [] -> failwith "Column not found."
    | h :: t ->
        if h.name = col_name then h.col_type else get_column_type_aux t name
  in
  get_column_type_aux table.columns col_name

(**Get list column names.*)
let get_column_names table =
  let rec get_cols_aux col_list =
    match col_list with [] -> [] | h :: t -> h.name :: get_cols_aux t
  in
  get_cols_aux table.columns

(*Construct row map.*)
let construct_row_map table row_data =
  let column_names = get_column_names table in
  if List.length column_names <> List.length row_data.values then
    failwith "Number of columns does not match number of elements in row."
  else
    let row_map = Hashtbl.create (List.length column_names + 1) in
    let rec build_map_aux cols row_data =
      match (cols, row_data) with
      | [], [] -> ()
      | col_h :: col_t, row_h :: row_t ->
          let _ = Hashtbl.add row_map col_h row_h in
          build_map_aux col_t row_t
      | _ -> failwith "Column/row mismatch."
    in
    let _ = build_map_aux column_names row_data.values in
    row_map

(**Get correct value.*)
let rec get_new_value_from_transform columns_lst values_lst column =
  match (columns_lst, values_lst) with
  | [], [] -> failwith "Column not found when creating new value in transform."
  | c :: c_t, v :: v_t ->
      if c = column then v else get_new_value_from_transform c_t v_t column
  | _ -> failwith "Column/row number mismatch creating new value in transform."

(**Construct a transform for update reassignments.*)
let construct_transform columns_lst values_lst table row_data =
  let column_names = get_column_names table in
  let rec transform_aux cols vals acc =
    match (cols, vals) with
    | [], [] -> List.rev acc
    | col_h :: col_t, val_h :: val_t ->
        let cur_val =
          if List.mem col_h columns_lst then
            get_new_value_from_transform columns_lst values_lst col_h
          else val_h
        in
        transform_aux col_t val_t (cur_val :: acc)
    | _ -> failwith "Column/row number mismatch when constructing transform."
  in
  { values = transform_aux column_names row_data.values [] }

(**Construct a predicate for filtering.*)
let construct_predicate columns_lst match_values_lst operators_lst table
    row_data =
  let row_map = construct_row_map table row_data in
  let rec pred_aux cols vals ops =
    match (cols, vals, ops) with
    | [], [], [] -> true
    | col_h :: col_t, val_h :: val_t, op_h :: op_t ->
        if op_h (Hashtbl.find row_map col_h) val_h = false then false
        else pred_aux col_t val_t op_t
    | _ -> failwith "Column/row number mismatch."
  in
  pred_aux columns_lst match_values_lst operators_lst

let create_table columns =
  let has_primary_key = List.exists (fun col -> col.primary_key) columns in
  if not has_primary_key then failwith "Table must have a primary key"
  else { columns; rows = [] }

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
    List.map2
      (fun col_name value ->
        let column = List.find (fun col -> col.name = col_name) table.columns in
        match String.trim value with
        | "" -> Null
        | v -> convert_to_value column.col_type v)
      column_names values
  in

  let new_row = row_values in
  table.rows <- { values = new_row } :: table.rows

let update_rows table pred f =
  table.rows <- List.map (fun r -> if pred r then f r else r) table.rows

let delete_rows table pred =
  table.rows <- List.filter (fun r -> not (pred r)) table.rows

let select_rows table column_names pred =
  let columns =
    List.map
      (fun name ->
        match List.find_opt (fun c -> c.name = name) table.columns with
        | Some c -> c
        | None -> failwith "Column does not exist")
      column_names
  in
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


(* Helper function to convert a value to a CSV-compatible string *)
let string_of_value = function
  | Int i -> string_of_int i
  | Varchar s -> "\"" ^ (String.escaped s) ^ "\""
  | Float f -> string_of_float f
  | Date d -> d 
  | Null -> ""

(* Serialize the row to CSV *)
let string_of_row row =
  String.concat "," (List.map string_of_value row.values)

(* Serialize the entire table to CSV *)
let serialize_table_to_csv table =
  String.concat "\n" (List.map string_of_row table.rows)

(* Write data to a file *)
let write_to_file ~filename ~data =
  let channel = open_out filename in
  output_string channel data;
  close_out channel

(* Function to convert and write a table to a CSV file *)
let write_table_to_csv table filename =
  let csv_data = serialize_table_to_csv table in
  write_to_file ~filename ~data:csv_data