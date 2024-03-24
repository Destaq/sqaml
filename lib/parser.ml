open Table
open Database
type token =
  | Identifier of string
  | IntKeyword
  | VarcharKeyword
  | PrimaryKey




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
          | _ -> Identifier hd
        in
        tokenize (token :: acc) tl
  in
  query
  |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> tokenize []

let parse_create_table tokens =
  let rec parse_columns acc = function
    | [] -> List.rev acc
    | Identifier name :: IntKeyword :: PrimaryKey :: PrimaryKey :: tl ->
        parse_columns ({ name; col_type = Int_type; primary_key = true } :: acc) tl
    | Identifier name :: IntKeyword ::  tl ->
        parse_columns ({ name; col_type = Int_type; primary_key = false } :: acc) tl
    | Identifier name :: VarcharKeyword :: tl ->
        parse_columns ({ name; col_type = Varchar_type; primary_key = false } :: acc) tl
    | Identifier ")" :: tl -> parse_columns acc tl
    | Identifier "(" :: tl -> parse_columns acc tl
    | Identifier "," :: tl -> parse_columns acc tl 
    | Identifier name :: PrimaryKey :: PrimaryKey ::  tl ->
        parse_columns ({ name; col_type = Int_type; primary_key = true } :: acc) tl
    | _ -> raise (Failure "Syntax error in column definition")
  in
  let rec parse_values acc = function
    | [] -> failwith "Syntax error in column definition"
    | Identifier "(" :: tl -> parse_values acc tl
    | Identifier ")" :: Identifier "VALUES" :: row_values -> (List.rev acc, row_values)
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
      let row_values, _ = parse_values [] row_values in
      insert_row _table_name columns row_values
  | Identifier  "SELECT" :: Identifier "*"  :: Identifier "FROM" :: Identifier _table_name :: [] ->
     select_all _table_name
  | _ -> raise (Failure "Syntax error in SQL query")
  
  
  

let parse_query query =
  let tokens = tokenize_query query in
  match tokens with
  | Identifier "CREATE" :: Identifier "TABLE" :: _ -> parse_create_table tokens
  | Identifier "INSERT" :: Identifier "INTO" :: _ -> parse_create_table tokens
  | Identifier "SELECT" :: _ -> parse_create_table tokens
  | _ -> raise (Failure "Unsupported query")

let print_tokenized tokens =
  List.iter (function
    | Identifier s -> Printf.printf "Identifier: %s\n" s
    | IntKeyword -> print_endline "IntKeyword"
    | VarcharKeyword -> print_endline "VarcharKeyword"
    | PrimaryKey -> print_endline "PrimaryKey"
  ) tokens

let parse_and_execute_query query = parse_query query
