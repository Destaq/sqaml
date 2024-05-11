(* storage.ml *)
open Table

let rec load_rows table columns = function
  | [] -> ()
  | h :: t ->
      Database.insert_row table columns h;
      load_rows table columns t

let fetch_files () =
  let list_files = Sys.readdir "lib/storage/" in
  List.filter
    (fun x -> Filename.extension x = ".csv")
    (Array.to_list list_files)

(**let print_2d_list lst =
  let max_lens =
    List.map (fun row -> List.map String.length row |> List.fold_left max 0) lst
  in
  let max_len = List.fold_left max 0 max_lens in
  List.iteri
    (fun _ row ->
      List.iteri (fun _ s -> Printf.printf "%*s " max_len s) row;
      print_newline ())
    lst*)

let string_to_col_type = function
  | "varchar" -> Varchar_type
  | "int" -> Int_type
  | _ -> failwith "Error. Incorrect column type saved."

let build_columns column_names column_types =
  let rec build_cols names types acc =
    match (names, types) with
    | [], [] -> List.rev acc
    | name :: rest_names, col_type :: rest_types ->
        let primary_key = match acc with [] -> true | _ -> false in
        let col =
          { name; col_type = string_to_col_type col_type; primary_key }
        in
        build_cols rest_names rest_types (col :: acc)
    | _ -> failwith "Column names and types have different lengths"
  in
  build_cols column_names column_types []

let header = function
  | names :: types :: _ -> build_columns names types
  | _ ->
      failwith
        "Storage format corrupted. No column names or types in storage. Please \
         purge the storage directory."

let load_table_from_file file =
  let table = Filename.remove_extension (Filename.basename file) in
  let data = Csv.square (Csv.load ("lib/storage/" ^ file)) in
  Database.create_table (header data) table;
  load_rows table
    (match data with
    | h :: _ -> h
    | _ ->
        failwith "Storage format corrupted. Header is not properly specified.")
    (match data with
    | _ :: _ :: data -> data
    | _ ->
        failwith "Storage format corrupted. Header is not properly specified.")

let rec load_tables = function
  | [] -> ()
  | h :: t ->
      load_table_from_file h;
      load_tables t

let load_from_storage () =
  let files = fetch_files () in
  load_tables files

let sync_on_exit () = ()
