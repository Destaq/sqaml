(* database.ml *)
open Table
open Row
(* Define a hash table mapping table names to references to table values *)
let tables : (string, table ref) Hashtbl.t = Hashtbl.create 10

(* Function to create a new table in the database *)
let create_table columns table_name =
  if Hashtbl.mem tables table_name then
    failwith "Table already exists"
  else
    let new_table = ref (create_table columns) in
    Hashtbl.add tables table_name new_table;
    print_table !new_table

(* Function to insert a row into a table *)
let insert_row table values row =
  if not (Hashtbl.mem tables table) then
    failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in  
    insert_row !table_ref values row;
    print_table !table_ref

(* Function to delete a table from the database *)

(* Function to update rows in a table *)
let update_rows table predicate transform =
  if not (Hashtbl.mem tables table) then
    failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    update_rows !table_ref predicate transform

(* Function to delete rows from a table *)
let delete_rows table predicate =
  if not (Hashtbl.mem tables table) then
    failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    delete_rows !table_ref predicate

(* Function to select rows from a table *)
let select_rows table fields predicate =
  if not (Hashtbl.mem tables table) then
    failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    select_rows !table_ref fields predicate
  
let select_all table =
  if not (Hashtbl.mem tables table) then
    failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    let rows = select_all !table_ref in 
    List.iter (fun row -> print_row row) rows
    

(* Function to print a table *)
let print_table table =
  if not (Hashtbl.mem tables table) then
    failwith "Table does not exist"
  else
    let table_ref = Hashtbl.find tables table in
    print_table !table_ref


