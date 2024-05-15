open Table
open Database

type token = Identifier of string | IntKeyword | VarcharKeyword | PrimaryKey

(**Print out string list [lst], with each element separated by [sep].*)
let rec print_string_list lst sep =
  match lst with
  | [] -> ()
  | h :: t ->
      let () = print_string (h ^ sep) in
      print_string_list t sep

(**Helper function for GitHub Actions, copy of List.find_index.*)
let find_index p =
  let rec aux i = function
    | [] -> None
    | a :: l -> if p a then Some i else aux (i + 1) l
  in
  aux 0

let print_tokenized tokens =
  List.iter
    (function
      | Identifier s -> Printf.printf "Identifier: %s\n" s
      | IntKeyword -> print_endline "IntKeyword"
      | VarcharKeyword -> print_endline "VarcharKeyword"
      | PrimaryKey -> print_endline "PrimaryKey")
    tokens

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

let rec quote_grouping acc cur_group in_group tokens =
  match tokens with
  | [] -> List.rev acc
  | h :: t ->
      if in_group then
        if String.ends_with ~suffix:"\"" h then
          quote_grouping
            ((cur_group ^ " " ^ replace_all h "\"" "") :: acc)
            "" false t
        else
          quote_grouping acc
            (cur_group ^ " " ^ replace_all h "\"" "")
            in_group t
      else if
        String.starts_with ~prefix:"\"" h
        && not (String.ends_with ~suffix:"\"" h)
      then quote_grouping acc (cur_group ^ " " ^ replace_all h "\"" "") true t
      else quote_grouping (replace_all h "\"" "" :: acc) cur_group in_group t

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
          | "SHOW" | "DROP" | "WHERE" | "UPDATE" | "SET" | "FROM" | "AND"
          | "ORDER" | "BY" | "LIMIT" | "COLUMNS" | "DELETE" ->
              Identifier (String.uppercase_ascii hd)
          | _ -> Identifier hd
        in
        tokenize (token :: acc) tl
  in
  query |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> quote_grouping [] "" false |> tokenize []

let check_column_order table_name columns =
  let actual_cols = get_table_columns table_name false in
  let rec check_equiv l1 l2 =
    match (l1, l2) with
    | h1 :: t1, h2 :: t2 ->
        if h1 = h2 then check_equiv t1 t2
        else failwith "Improper columns or order provided for insert."
    | [], [] -> ()
    | _ -> failwith "Incorrect number of columns provided."
  in
  check_equiv actual_cols columns

let parse_create_table tokens =
  let rec parse_columns acc = function
    | [] -> List.rev acc
    | Identifier name :: IntKeyword :: PrimaryKey :: PrimaryKey :: tl ->
        parse_columns
          ({ name; col_type = Int_type; primary_key = true } :: acc)
          tl
    | Identifier name :: VarcharKeyword :: PrimaryKey :: PrimaryKey :: tl ->
        parse_columns
          ({ name; col_type = Varchar_type; primary_key = true } :: acc)
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
  in
  let rec parse_values acc = function
    | [] -> failwith "Syntax error in column definition"
    | Identifier "(" :: tl -> parse_values acc tl
    | Identifier ")" :: Identifier "VALUES" :: row_values ->
        (List.rev acc, row_values)
    | Identifier ")" :: _ -> (List.rev acc, [])
    | Identifier "," :: tl -> parse_values acc tl
    | Identifier name :: tl -> parse_values (name :: acc) tl
    | _ -> raise (Failure "Syntax error in column definition")
  in
  match tokens with
  | Identifier "CREATE" :: Identifier "TABLE" :: Identifier _table_name :: tl ->
      let columns = parse_columns [] tl in
      create_table columns _table_name
  | Identifier "INSERT" :: Identifier "INTO" :: Identifier _table_name :: tl ->
      let columns, row_values = parse_values [] tl in
      let () = check_column_order _table_name columns in
      let row_values, _ = parse_values [] row_values in
      let pk_field = get_pk_field _table_name in

      if Option.is_some pk_field then
        let pk_field = Option.get pk_field in
        let () =
          check_pk_uniqueness _table_name pk_field.name
            (Table.convert_to_value pk_field.col_type
               (List.nth row_values
                  (Option.get (find_index (fun c -> c = pk_field.name) columns))))
        in
        insert_row _table_name columns row_values
      else insert_row _table_name columns row_values
  | _ -> raise (Failure "Syntax error in SQL query")

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
    | (Identifier "ORDER" | Identifier "LIMIT") :: _remaining_tokens ->
        (col_acc, val_acc, op_acc)
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

let rec parse_select_query_fields tokens acc =
  match tokens with
  | [] -> failwith "Please include fields in your query."
  | h :: t -> (
      match h with
      | Identifier cur_tok ->
          if cur_tok = "FROM" then
            ( acc,
              match List.hd t with
              | Identifier _tb_name -> _tb_name
              | _ -> failwith "No table name detected." )
          else parse_select_query_fields t (h :: acc)
      | _ ->
          failwith "Non-identifier detected whil parsing select query fields.")

let rec extract_column_names tb_name fields =
  match fields with
  | [] -> []
  | h :: t -> (
      match h with
      | Identifier cur_tok ->
          if cur_tok = "*" then get_table_columns tb_name false
          else if cur_tok <> "," then cur_tok :: extract_column_names tb_name t
          else extract_column_names tb_name t
      | _ -> failwith "Non-identifier detected in column list.")

let rec get_limit_info select_tokens =
  match select_tokens with
  | Identifier "LIMIT" :: Identifier lim :: _ -> (true, int_of_string lim)
  | _ :: t -> get_limit_info t
  | [] -> (false, 0)

let rec get_order_by_info select_tokens =
  match select_tokens with
  | Identifier "ORDER"
    :: Identifier "BY"
    :: Identifier col
    :: Identifier dir
    :: _ ->
      let dir = String.uppercase_ascii dir in
      if dir = "ASC" || dir = "DESC" then (true, col, dir)
      else failwith "Order by direction not provided."
  | _ :: t -> get_order_by_info t
  | _ -> (false, "", "")

let rec take n xs =
  if not (List.length xs = 0) then
    match n with 0 -> [] | _ -> List.hd xs :: take (n - 1) (List.tl xs)
  else []

let construct_sorter table_name column_ind r1 r2 =
  sorter table_name column_ind r1 r2

let parse_select_records select_tokens =
  let ordered, order_column, order_dir = get_order_by_info select_tokens in
  let limited, limit = get_limit_info select_tokens in
  let selected_fields, table_name =
    parse_select_query_fields select_tokens []
  in
  let selected_fields = extract_column_names table_name selected_fields in
  let () =
    if
      order_column <> "" && order_column <> ""
      && not (List.mem order_column selected_fields)
    then failwith "Order column is not present in field list."
    else ()
  in
  let () =
    if List.length selected_fields = 0 then
      failwith "No proper fields selected in query."
    else ()
  in
  let has_where, where_clause = includes_where_clause select_tokens in
  let order_ind, selected_rows =
    if has_where then
      let columns_lst, values_lst, ops_lst =
        construct_predicate_params table_name where_clause
      in
      let pred =
        construct_predicate columns_lst values_lst ops_lst table_name
      in
      select_rows table_name selected_fields pred order_column
    else select_rows table_name selected_fields (fun _ -> true) order_column
  in
  let selected_rows =
    if ordered && Option.is_some order_ind then
      if order_dir = "DESC" then
        List.rev
          (List.sort
             (construct_sorter table_name (Option.get order_ind))
             selected_rows)
      else
        List.sort
          (construct_sorter table_name (Option.get order_ind))
          selected_rows
    else selected_rows
  in
  let selected_rows =
    if limited then take limit selected_rows else selected_rows
  in
  List.iter (fun row -> Row.print_row row) selected_rows

let parse_query query =
  let query = replace_all query "," " , " in
  let query = replace_all query "(" " ( " in
  let query = replace_all query ")" " ) " in
  let query = replace_all query "`" "" in
  let query = replace_all query "'" "" in
  let query = replace_all query "\n" "" in
  let query = replace_all query "\r" "" in
  let tokens = tokenize_query query in
  match tokens with
  | Identifier "CREATE" :: Identifier "TABLE" :: _ -> parse_create_table tokens
  | Identifier "SHOW"
    :: Identifier "COLUMNS"
    :: Identifier "FROM"
    :: Identifier _table_name
    :: _ ->
      print_string_list (get_table_columns _table_name true) "|";
      print_string "\n"
  | Identifier "INSERT" :: Identifier "INTO" :: _ -> parse_create_table tokens
  | Identifier "SELECT" :: select_tokens -> parse_select_records select_tokens
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
