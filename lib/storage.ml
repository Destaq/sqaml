(* storage.ml *)
open Table
open Database

(**Load rows into the database on start.*)
let rec load_rows table columns = function
  | [] -> ()
  | h :: t ->
      Database.insert_row table columns h;
      load_rows table columns t

(**Fetch data/table storage files.*)
let fetch_files () =
  try
    let list_files = Sys.readdir "lib/storage/" in
    List.filter
      (fun x -> Filename.extension x = ".sqaml")
      (Array.to_list list_files)
  with Sys_error _ ->
    Sys.mkdir "lib/storage/" 0o777;
    []

(**Remove all files in a directory.*)
let remove_all_files_in_dir dir =
  try
    let files = Array.to_list (Sys.readdir dir) in
    List.iter
      (fun file ->
        let file_path = Filename.concat dir file in
        if Sys.is_directory file_path then () else Sys.remove file_path)
      files
  with Sys_error msg -> Printf.eprintf "Error: %s\n" msg

(**Convert a string to a corresponding column type.*)
let string_to_col_type = function
  | "varchar" -> Varchar_type
  | "int" -> Int_type
  | _ -> failwith "Error. Incorrect column type saved."

(**Build out table columns on load, after file parsing and data extraction.*)
let build_columns column_names column_types primary_key =
  let rec build_cols names types acc =
    match (names, types) with
    | [], [] -> List.rev acc
    | name :: rest_names, col_type :: rest_types ->
        let col =
          {
            name;
            col_type = string_to_col_type col_type;
            primary_key = primary_key = name;
          }
        in
        build_cols rest_names rest_types (col :: acc)
    | _ -> failwith "Column names and types have different lengths"
  in
  build_cols column_names column_types []

(**Parse the header of a storage file.*)
let header pk = function
  | names :: types :: _ -> build_columns names types pk
  | _ ->
      failwith
        "Storage format corrupted. No column names or types in storage. Please \
         purge the storage directory."

(**Basic string utility function for extracting parts of a storage file.*)
let split_and_get_parts s =
  let parts = String.split_on_char '_' s in
  match parts with
  | [] -> ("", "")
  | [ part ] -> (part, "")
  | part1 :: part2 :: _ -> (part1, part2)

(**Main function to load a table from a specific storage file.*)
let load_table_from_file file =
  let filename = Filename.remove_extension (Filename.basename file) in
  let table = fst (split_and_get_parts filename) in
  let pk = snd (split_and_get_parts filename) in
  let data = Csv.square (Csv.load ("lib/storage/" ^ file)) in
  Database.create_table (header pk data) table;
  load_rows table
    (match data with
    | h :: _ -> h
    | _ ->
        failwith "Storage format corrupted. Header is not properly specified.")
    (match data with
    | _ :: _ :: data -> data
    | _ ->
        failwith "Storage format corrupted. Header is not properly specified.")

(**Load tables from directory.*)
let rec load_tables = function
  | [] -> ()
  | h :: t ->
      load_table_from_file h;
      load_tables t

(**Fetch all storage files and load tables.*)
let load_from_storage () =
  let files = fetch_files () in
  load_tables files

(**Utility function to extract all keys names from a hashtable.*)
let get_keys_from_hashtbl hashtbl =
  let keys = ref [] in
  Hashtbl.iter (fun key _ -> keys := key :: !keys) hashtbl;
  List.rev !keys

(**Convert SQamL database rows to lists. *)
let rec rows_to_lists = function
  | [] -> []
  | h :: t -> Row.to_list h :: rows_to_lists t

(**Convert column types back into string formats for saving in storage files.*)
let rec types_from_names table_name = function
  | [] -> []
  | h :: t ->
      (match get_column_type table_name h with
      | Int_type -> "int"
      | Varchar_type -> "varchar"
      | _ -> "Incorrect column type")
      :: types_from_names table_name t

(**Get the name of the primary key in a field.*)
let get_pk_field_name table =
  match get_pk_field table with
  | None -> failwith "No primary key."
  | Some x -> x.name

(**Save data from tables into specific storage files (direct from main database).*)
let rec save_data = function
  | [] -> ()
  | h :: t ->
      let names = get_columns_lst !(Hashtbl.find tables h) false in
      let types = types_from_names h names in
      let pk = get_pk_field_name h in
      Csv.save
        ("lib/storage/" ^ h ^ "_" ^ pk ^ ".sqaml")
        (names :: types
        :: rows_to_lists (Table.select_all !(Hashtbl.find tables h)));
      save_data t

(**Sync the database to storage files on exit.*)
let sync_on_exit () =
  remove_all_files_in_dir "lib/storage/";
  save_data (get_keys_from_hashtbl tables)
