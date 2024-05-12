(* database.ml *)
open Table
open Row

(* Define a hash table mapping table names to references to table values *)
let tables : (string, table ref) Hashtbl.t = Hashtbl.create 10

(* Function to print the names of all loaded tables.*)
let show_all_tables () =
  let table_names = Hashtbl.fold (fun k _ acc -> k :: acc) tables [] in
  if List.length table_names > 0 then
    let () = print_string "Tables:\n" in
    List.iter (fun name -> print_string (name ^ "\n")) table_names
  else print_string "No tables in database.\n"

(**Get primary key field from a table.*)
let get_pk_field table =
  if not (Hashtbl.mem tables table) then failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    get_table_pk_field !table_ref

(**Check primary key uniqueness*)
let check_pk_uniqueness table pk_field pk_value =
  if not (Hashtbl.mem tables table) then failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    check_for_pk_value !table_ref pk_field pk_value

(**Get all columns from a table.*)
let get_table_columns table include_type =
  if not (Hashtbl.mem tables table) then failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    get_columns_lst !table_ref include_type

(**Get column type from table name*)
let get_column_type table column =
  if not (Hashtbl.mem tables table) then failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    get_column_type !table_ref column

(**Construct a transformation function for data updates.*)
let construct_transform columns_lst values_lst table row_data =
  if not (Hashtbl.mem tables table) then failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    construct_transform columns_lst values_lst !table_ref row_data

(**Construct predicate for where clauses*)
let construct_predicate columns_lst match_values_lst operators_lst table
    row_data =
  if not (Hashtbl.mem tables table) then failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    construct_predicate columns_lst match_values_lst operators_lst !table_ref
      row_data

(**Function to drop a table from the database.*)
let drop_table table_name = Hashtbl.remove tables table_name

(* Function to create a new table in the database *)
let create_table columns table_name =
  if Hashtbl.mem tables table_name then failwith "Table already exists"
  else
    let new_table = ref (create_table columns) in
    Hashtbl.add tables table_name new_table

(* Function to insert a row into a table *)
let insert_row table values row =
  if not (Hashtbl.mem tables table) then failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    insert_row !table_ref values row

(* Function to update rows in a table *)
let update_rows table predicate transform =
  if not (Hashtbl.mem tables table) then failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    update_rows !table_ref predicate transform

(* Function to delete rows from a table *)
let delete_rows table predicate =
  if not (Hashtbl.mem tables table) then failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    delete_rows !table_ref predicate

(* Function to select rows from a table *)
let select_rows table fields predicate order_col =
  if not (Hashtbl.mem tables table) then failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    let selected_rows =
      if List.length fields = 1 && List.hd fields = "*" then
        select_rows_table !table_ref
          (get_columns_lst !table_ref false)
          predicate order_col
      else select_rows_table !table_ref fields predicate order_col
    in
    selected_rows

let select_all table =
  if not (Hashtbl.mem tables table) then failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    let rows = select_all !table_ref in
    List.iter (fun row -> print_row row) rows

(* Function to print a table *)
let print_table table =
  if not (Hashtbl.mem tables table) then failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    print_table !table_ref

(**Compare two rows based on column name.*)
let sorter table col_ind r1 r2 =
  if not (Hashtbl.mem tables table) then failwith "Table does not exist"
  else compare_row col_ind r1 r2
