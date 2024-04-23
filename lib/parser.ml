open Table
open Database

type token = Identifier of string | IntKeyword | VarcharKeyword | PrimaryKey

let print_tokenized tokens =
  List.iter
    (function
      | Identifier s -> Printf.printf "Identifier: %s\n" s
      | IntKeyword -> print_endline "IntKeyword"
      | VarcharKeyword -> print_endline "VarcharKeyword"
      | PrimaryKey -> print_endline "PrimaryKey")
    tokens

let tokenize_query query =
  let rec tokenize acc = function
    | [] -> List.rev acc
    | hd :: tl ->
        let token =
          match String.uppercase_ascii hd with
          | "INT" -> IntKeyword
          | "VARCHAR" -> VarcharKeyword
          | "PRIMARY" -> PrimaryKey
          | "KEY" -> PrimaryKey
          | "TABLE" | "TABLES" | "CREATE" | "INSERT" | "INTO" | "SELECT"
          | "SHOW" | "DROP" | "WHERE" ->
              Identifier (String.uppercase_ascii hd)
          | _ -> Identifier hd
        in
        tokenize (token :: acc) tl
  in
  query |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> tokenize []

let rec includes_where_clause tokens =
  match tokens with
  | [] -> (false, [])
  | h :: t -> (
      match h with
      | Identifier cur_tok ->
          if cur_tok = "WHERE" then (true, h :: t) else includes_where_clause t
      | _ -> includes_where_clause t)

let get_update_fields_clause all_tokens =
  let rec get_update_fields_clause_aux tokens acc =
    match tokens with
    | [] -> List.rev acc
    | h :: t -> (
        match h with
        | Identifier cur_tok ->
            if cur_tok = "WHERE" then List.rev acc
            else get_update_fields_clause_aux t (Identifier cur_tok :: acc)
        | _ -> failwith "Unrecognized update clause query.")
  in
  get_update_fields_clause_aux all_tokens []

let get_op_value op =
  match op with
  | "=" -> Row.value_equals
  | ">" -> Row.value_greater_than
  | "<" -> Row.value_less_than
  | "<>" -> Row.value_not_equals
  | _ -> failwith "Unrecognized operation string."

let construct_predicate_params table_name pred_tokens =
  let pred_tokens =
    List.filter
      (fun elem -> match elem with Identifier "," -> false | _ -> true)
      pred_tokens
  in
  let rec construct_pred_aux tokens col_acc val_acc op_acc =
    match tokens with
    | [] -> (col_acc, val_acc, op_acc)
    | (Identifier "WHERE" | Identifier "AND")
      :: Identifier field1
      :: Identifier op
      :: Identifier value1
      :: _remaining_tokens ->
        construct_pred_aux _remaining_tokens (field1 :: col_acc)
          (Table.convert_to_value (get_column_type table_name field1) value1
          :: val_acc)
          (get_op_value op :: op_acc)
    | _ -> failwith "Unrecognized where clause format."
  in
  construct_pred_aux pred_tokens [] [] []

let construct_transform_params table_name update_tokens =
  let update_tokens = get_update_fields_clause update_tokens in
  let update_tokens =
    List.filter
      (fun elem -> match elem with Identifier "," -> false | _ -> true)
      update_tokens
  in
  let rec construct_transform_aux tokens col_acc val_acc =
    match tokens with
    | [] -> (col_acc, val_acc)
    | Identifier field1
      :: Identifier "="
      :: Identifier value1
      :: _remaining_tokens ->
        construct_transform_aux _remaining_tokens (field1 :: col_acc)
          (Table.convert_to_value (get_column_type table_name field1) value1
          :: val_acc)
    | _ -> failwith "Unrecognized update transform clause format."
  in
  construct_transform_aux update_tokens [] []

(**Need to fix first transform line*)
let parse_update_table table_name update_tokens =
  let transform_columns_lst, transform_values_lst =
    construct_transform_params table_name update_tokens
  in
  let transform =
    construct_transform transform_columns_lst transform_values_lst table_name
  in
  let has_where, where_clause = includes_where_clause update_tokens in
  if has_where then
    let columns_lst, values_lst, ops_lst =
      construct_predicate_params table_name where_clause
    in
    let pred = construct_predicate columns_lst values_lst ops_lst table_name in
    update_rows table_name pred transform
  else update_rows table_name (fun _ -> true) transform

let parse_delete_records table_name delete_tokens =
  let has_where, where_clause = includes_where_clause delete_tokens in
  if has_where then
    let columns_lst, values_lst, ops_lst =
      construct_predicate_params table_name where_clause
    in
    let pred = construct_predicate columns_lst values_lst ops_lst table_name in
    delete_rows table_name pred
  else delete_rows table_name (fun _ -> true)

let replace_all str old_substring new_substring =
  let rec replace_helper str old_substring new_substring start_pos =
    try
      let pos = String.index_from str start_pos old_substring.[0] in
      if String.sub str pos (String.length old_substring) = old_substring then
        let prefix = String.sub str 0 pos in
        let suffix =
          String.sub str
            (pos + String.length old_substring)
            (String.length str - (pos + String.length old_substring))
        in
        let new_str = prefix ^ new_substring ^ suffix in
        replace_helper new_str old_substring new_substring
          (pos + String.length new_substring)
      else replace_helper str old_substring new_substring (pos + 1)
    with Not_found -> str
  in
  replace_helper str old_substring new_substring 0

let rec parse_columns acc = function
  | [] -> List.rev acc
  | Identifier name :: IntKeyword :: PrimaryKey :: PrimaryKey :: tl ->
      parse_columns
        ({ name; col_type = Int_type; primary_key = true } :: acc)
        tl
  | Identifier name :: IntKeyword :: tl ->
      parse_columns
        ({ name; col_type = Int_type; primary_key = false } :: acc)
        tl
  | Identifier name :: VarcharKeyword :: tl ->
      parse_columns
        ({ name; col_type = Varchar_type; primary_key = false } :: acc)
        tl
  | Identifier ")" :: tl -> parse_columns acc tl
  | Identifier "(" :: tl -> parse_columns acc tl
  | Identifier "," :: tl -> parse_columns acc tl
  | Identifier name :: PrimaryKey :: PrimaryKey :: tl ->
      parse_columns
        ({ name; col_type = Int_type; primary_key = true } :: acc)
        tl
  | _ -> raise (Failure "Syntax error in column definition")

let rec extract_columns acc = function
  | Identifier "FROM" :: _ as rest -> (List.rev acc, rest)
  | Identifier col :: Identifier "," :: tl -> extract_columns (col :: acc) tl
  | Identifier col :: tl -> extract_columns (col :: acc) tl
  | _ -> failwith "Syntax error in SELECT columns"

let rec parse_values acc = function
  | [] -> failwith "Syntax error in column definition"
  | Identifier "(" :: tl -> parse_values acc tl
  | Identifier ")" :: Identifier "VALUES" :: row_values ->
      (List.rev acc, row_values)
  | Identifier ")" :: _ -> (List.rev acc, [])
  | Identifier "," :: tl -> parse_values acc tl
  | Identifier name :: tl -> parse_values (name :: acc) tl
  | _ -> raise (Failure "Syntax error in column definition")

let parse_create_table tokens =
  match tokens with
  | Identifier "CREATE" :: Identifier "TABLE" :: Identifier _table_name :: tl ->
      let columns = parse_columns [] tl in
      create_table columns _table_name
  | Identifier "INSERT" :: Identifier "INTO" :: Identifier _table_name :: tl ->
      let columns, row_values = parse_values [] tl in
      let row_values, _ = parse_values [] row_values in
      insert_row _table_name columns row_values
  | [
   Identifier "SELECT";
   Identifier "*";
   Identifier "FROM";
   Identifier _table_name;
  ] ->
      select_all _table_name
  | [
   Identifier "SELECT";
   Identifier "*";
   Identifier "FROM";
   Identifier _table_name;
   Identifier "WHERE";
   Identifier field1;
   Identifier op;
   Identifier value1;
  ] ->
      let columns_lst, values_lst, ops_lst =
        construct_predicate_params _table_name
          [
            Identifier "WHERE";
            Identifier field1;
            Identifier op;
            Identifier value1;
          ]
      in
      let pred =
        construct_predicate columns_lst values_lst ops_lst _table_name
      in
      select_rows _table_name (get_column_names _table_name) pred
  | Identifier "SELECT" :: tl -> (
      let columns, rest = extract_columns [] tl in
      match rest with
      | Identifier "FROM" :: Identifier table_name :: rest_conditions ->
          let predicate =
            if
              List.exists
                (function Identifier "WHERE" -> true | _ -> false)
                rest_conditions
            then
              let columns_lst, values_lst, ops_lst =
                construct_predicate_params table_name rest_conditions
              in
              let pred =
                construct_predicate columns_lst values_lst ops_lst table_name
              in
              pred
            else fun _ -> true
          in
          select_rows table_name columns predicate
      | _ -> raise (Failure "Syntax error in SELECT query"))
  | _ -> raise (Failure "Syntax error in SQL query")

let parse_query query =
  let query = replace_all query "," " , " in
  let query = replace_all query "(" " ( " in
  let query = replace_all query ")" " ) " in
  let tokens = tokenize_query query in
  match tokens with
  | Identifier "CREATE" :: Identifier "TABLE" :: _ -> parse_create_table tokens
  | Identifier "INSERT" :: Identifier "INTO" :: _ -> parse_create_table tokens
  | Identifier "SELECT" :: _ -> parse_create_table tokens
  | Identifier "SHOW" :: Identifier "TABLES" :: _ -> show_all_tables ()
  | Identifier "DROP" :: Identifier "TABLE" :: Identifier _table_name :: _ ->
      drop_table _table_name
  | Identifier "UPDATE"
    :: Identifier _table_name
    :: Identifier "SET"
    :: update_tokens ->
      parse_update_table _table_name update_tokens
  | Identifier "DELETE"
    :: Identifier "FROM"
    :: Identifier _table_name
    :: delete_tokens ->
      parse_delete_records _table_name delete_tokens
  | _ -> raise (Failure "Unsupported query")

let parse_and_execute_query query = parse_query query
