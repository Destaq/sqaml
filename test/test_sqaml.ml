open OUnit2

(** [printer_wrapper s] is [s] *)
let printer_wrapper s = s

(* TODO: investigate how to do this without Unix. *)

(** [with_redirected_stdout f] is the output of [f] with stdout redirected to a
    temporary file, useful for checking the output of some printing. *)
let with_redirected_stdout f =
  (* clear any existing stdout *)
  flush stdout;
  let original_stdout = Unix.dup Unix.stdout in
  let temp_out = open_out "temp_out" in
  Unix.dup2 (Unix.descr_of_out_channel temp_out) Unix.stdout;
  f ();
  flush stdout;
  Unix.dup2 original_stdout Unix.stdout;
  close_out temp_out;
  let temp_in = open_in "temp_out" in
  let rec read_all_lines acc =
    try
      let line = input_line temp_in in
      read_all_lines (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let output = read_all_lines "" in
  output

(** [create_tables ()] creates two tables, "test_table" and "another_table",
    for the global state. *)
let create_tables () =
  Sqaml.Database.create_table
    [
      { name = "example"; col_type = Sqaml.Table.Int_type; primary_key = true };
      {
        name = "example2";
        col_type = Sqaml.Table.Date_type;
        primary_key = false;
      };
      {
        name = "example3";
        col_type = Sqaml.Table.Float_type;
        primary_key = false;
      };
      {
        name = "example4";
        col_type = Sqaml.Table.Null_type;
        primary_key = false;
      };
    ]
    "test_table";
  Sqaml.Database.create_table
    [ { name = "hi"; col_type = Sqaml.Table.Float_type; primary_key = true } ]
    "another_table";
  ()

(** [as_test name f] is an OUnit test with name [name] that runs [f]. *)
let as_test name f = name >:: fun _ -> f ()

(** [drop_tables ()] drops all tables in the global state. *)
let drop_tables () =
  Sqaml.Database.drop_table "test_table";
  Sqaml.Database.drop_table "another_table";
  ()

(** [test_show_all_tables_with_no_tables] is an OUnit test that checks that
   [show_all_tables] returns the correct output when there are no tables in the
   database. *)
let test_show_all_tables_with_no_tables =
  as_test "test_show_all_tables_with_no_tables" (fun () ->
      drop_tables ();
      let output = with_redirected_stdout Sqaml.Database.show_all_tables in
      let () = print_endline "GOING FOR IT" in
      let () = print_endline output in
      let () = print_int (String.length output) in
      (* TODO: investigate why it doesn't always show no tables in database... *)
      assert_bool "bad_show_no_tables"
        (output = "No tables in database.\n" || String.length output = 0))

(** [test_show_all_tables_with_some_tables] is an OUnit test that checks that
   [show_all_tables] returns the correct output when there are some tables in the database. *)
let test_show_all_tables_with_some_tables =
  as_test "test_show_all_tables_with_some_tables" (fun () ->
      create_tables ();
      let output = with_redirected_stdout Sqaml.Database.show_all_tables in
      assert_equal ~printer:printer_wrapper
        "Tables:\nanother_table\ntest_table\n" output;
      drop_tables ())

(** [test_get_column_type_column_present] is an OUnit test that checks that
   [get_column_type] returns the correct column type when the column is present. *)
let test_get_column_type_column_present =
  as_test "test_get_column_type_column_present" (fun () ->
      create_tables ();
      let output = Sqaml.Database.get_column_type "test_table" "example" in
      assert_equal output Sqaml.Table.Int_type;
      drop_tables ())

(** [test_get_column_type_table_absent] is an OUnit test that checks that [get_column_type]
    raises a custom Failure when the table is absent. *)
let test_get_column_type_table_absent =
  as_test "test_get_column_type_table_absent" (fun () ->
      create_tables ();
      let failure_fun () =
        Sqaml.Database.get_column_type "no_table" "nonexistent"
      in
      OUnit2.assert_raises (Failure "Table does not exist") failure_fun;
      drop_tables ())

(** [test_get_column_type_column_absent] is an OUnit test that checks that [get_column_type]
    raises a custom Failure when the asked-for column is absent. *)
let test_get_column_type_column_absent =
  as_test "test_get_column_type_column_absent" (fun () ->
      create_tables ();
      try
        let _ = Sqaml.Database.get_column_type "test_table" "nonexistent" in
        drop_tables ();
        assert_failure "Expected failure for nonexistent column, but got none."
      with
      | Failure msg ->
          drop_tables ();
          assert_equal ~printer:printer_wrapper "Column not found." msg
      | _ ->
          drop_tables ();
          assert_failure
            "Expected Failure exception, but got different exception.")

(** [test_construct_transform_column_present] is an OUnit test that verifies
    the correctness of [Sqaml.Database.construct_transform], a row-updating function. *)
let test_construct_transform_column_present =
  as_test "test_construct_transform_column_present" (fun () ->
      create_tables ();
      let updated_row =
        Sqaml.Database.construct_transform
          [ "example"; "example2"; "example3"; "example4" ]
          [ Int 1; Date "2022-12-12"; Float 4.5; Null ]
          "test_table"
          { values = [ Int 0; Date "2022-12-12"; Float 4.5; Null ] }
      in
      assert_equal updated_row.values
        [ Int 1; Date "2022-12-12"; Float 4.5; Null ];
      drop_tables ())

(** [test_construct_transform_table_absent] is an OUnit test that verifies that
    [construct_transform] raises a custom Failure when the table is absent. *)
let test_construct_transform_table_absent =
  as_test "test_construct_transform_table_absent" (fun () ->
      let updated_row () =
        Sqaml.Database.construct_transform [ "example" ] [ Int 1 ] "test_table"
          { values = [ Int 0 ] }
      in
      assert_raises (Failure "Table does not exist") updated_row)

(** [test_construct_predicate_column_present] is an OUnit test that operates
    in a similar manner to [test_construct_transform_column_present], but
    verifies the correctness of [Sqaml.Database.construct_predicate] instead. *)
let test_construct_predicate_column_present =
  as_test "test_construct_predicate_column_present" (fun () ->
      create_tables ();
      let predicate =
        Sqaml.Database.construct_predicate
          [ "example"; "example2"; "example3"; "example4" ]
          [ Int 1; Date "2022-12-12"; Float 4.5; Null ]
          [ (fun (x : Sqaml.Row.value) (y : Sqaml.Row.value) -> x > y) ]
          "test_table"
      in
      let result =
        predicate { values = [ Int 0; Date "2022-12-12"; Float 4.5; Null ] }
      in
      assert_equal result false;
      drop_tables ())

(** [test_construct_predicate_table_absent] is an OUnit test that verifies that
    [construct_predicate] raises a custom Failure when the table is absent. *)
let test_construct_predicate_table_absent =
  as_test "test_construct_predicate_table_absent" (fun () ->
      drop_tables ();
      let predicate =
        Sqaml.Database.construct_predicate [ "example" ] [ Int 1 ]
          [ (fun (x : Sqaml.Row.value) (y : Sqaml.Row.value) -> x > y) ]
          "asdfsdfsdf"
      in
      assert_raises (Failure "Table does not exist") (fun () ->
          predicate { values = [ Int 0 ] }))

(** [test_insert_row_table_exists] is an OUnit test that checks that
    [Sqaml.Database.insert_row] correctly inserts a row into a table that
    exists. *)
let test_insert_row_table_exists =
  as_test "test_insert_row_table_exists" (fun () ->
      create_tables ();
      let values = [ "17"; "2022-12-12"; "4.5"; "null" ] in
      let output =
        with_redirected_stdout (fun () ->
            Sqaml.Database.insert_row "test_table"
              [ "example"; "example2"; "example3"; "example4" ]
              values;
            Sqaml.Database.delete_rows "test_table" (fun _ -> true))
      in
      assert_equal ~printer:printer_wrapper
        "example: int\n\
         example2: date\n\
         example3: float\n\
         example4: null\n\
         17 2022-12-12 4.500000 NULL \n"
        output;
      drop_tables ())

(** [test_insert_row_table_absent] is an OUnit test that checks that
    [Sqaml.Database.insert_row] raises a custom Failure when the table does
    not exist. *)
let test_insert_row_table_absent =
  as_test "test_insert_row_table_absent" (fun () ->
      let values = [ "12" ] in
      let insert_absent_table () =
        Sqaml.Database.insert_row "test_table" [ "example" ] values
      in
      assert_raises (Failure "Table does not exist") insert_absent_table)

(** [test_create_table_already_exists] is an OUnit test that checks that
    [Sqaml.Database.create_table] raises a custom Failure when the table
    already exists. *)
let test_create_table_already_exists =
  as_test "test_create_table_already_exists" (fun () ->
      create_tables ();
      let create_table () =
        Sqaml.Database.create_table
          [
            {
              name = "example";
              col_type = Sqaml.Table.Int_type;
              primary_key = true;
            };
          ]
          "test_table"
      in
      assert_raises (Failure "Table already exists") create_table;
      drop_tables ())

(** [test_delete_rows_nonexistent_table] is an OUnit test that checks that
    [Sqaml.Database.delete_rows] raises a custom Failure when the table does
    not exist. *)
let test_delete_rows_nonexistent_table =
  as_test "test_delete_rows_nonexistent_table" (fun () ->
      drop_tables ();
      let delete_rows () =
        Sqaml.Database.delete_rows "nonexistent" (fun _ -> true)
      in
      assert_raises (Failure "Table does not exist") delete_rows)

(** [test_update_rows_nonexistent_table] is an OUnit test that checks that
    [Sqaml.Database.update_rows] raises a custom Failure when the table does
    not exist. *)
let test_update_rows_nonexistent_table =
  as_test "test_update_rows_nonexistent_table" (fun () ->
      drop_tables ();
      let update () =
        Sqaml.Database.update_rows "example"
          (fun _ -> true)
          (fun _ -> { values = [ Int 1 ] })
      in
      assert_raises (Failure "Table does not exist") update)

(** [test_normal_update_rows] is an OUnit test that checks that
    [Sqaml.Database.update_rows] correctly updates rows in a table. *)
let test_normal_update_rows =
  as_test "test_normal_update_rows" (fun () ->
      create_tables ();
      Sqaml.Database.insert_row "test_table" [ "example" ] [ "0" ];
      let output =
        with_redirected_stdout (fun () ->
            Sqaml.Database.update_rows "test_table"
              (fun row -> row.values = [ Int 0 ])
              (fun _ -> { values = [ Int 1 ] });
            Sqaml.Database.select_all "test_table")
      in
      assert_equal ~printer:printer_wrapper "1 \n" output;
      drop_tables ())

(** [test_missing_select_all_table] is an OUnit test that checks that
    [Sqaml.Database.select_all] raises a custom Failure when the table does
    not exist. *)
let test_missing_select_all_table =
  as_test "test_missing_select_all_table" (fun () ->
      drop_tables ();
      let select_all () = Sqaml.Database.select_all "nonexistent" in
      assert_raises (Failure "Table does not exist") select_all)

(** [test_print_table] serves to see if [Sqaml.Database.print_table] prints
    the correct representation of some table, with its type and data. *)
let test_print_table =
  as_test "test_normal_update_rows" (fun () ->
      create_tables ();
      Sqaml.Database.insert_row "test_table" [ "example" ] [ "0" ];
      let output =
        with_redirected_stdout (fun () ->
            Sqaml.Database.update_rows "test_table"
              (fun row -> row.values = [ Int 0 ])
              (fun _ -> { values = [ Int 1 ] });
            Sqaml.Database.print_table "test_table")
      in
      assert_equal ~printer:printer_wrapper
        "example: int\nexample2: date\nexample3: float\nexample4: null\n1 \n"
        output;
      drop_tables ())

(** [test_print_nonexistent_table] is an OUnit test that checks that
    [Sqaml.Database.print_table] raises a custom Failure when the table does
    not exist. *)
let test_print_nonexistent_table =
  as_test "test_print_nonexistent_table" (fun () ->
      drop_tables ();
      let print_table () = Sqaml.Database.print_table "nonexistent" in
      assert_raises (Failure "Table does not exist") print_table)

(** [test_select_rows] is an OUnit test that checks that [Sqaml.Database.select_rows]
    returns the correct rows when the table exists. *)
let test_select_rows =
  as_test "test_select_rows" (fun () ->
      create_tables ();
      Sqaml.Database.insert_row "test_table"
        [ "example"; "example2"; "example3"; "example4" ]
        [ "0"; "2022-12-12"; "4.5"; "null" ];
      assert_equal
        (Sqaml.Database.select_rows "test_table" [ "example" ]
           (fun _ -> true)
           "")
        (None, [ { values = [ Int 0 ] } ]);
      drop_tables ())

(** [test_select_rows_nonexistent_table] is an OUnit test that checks that
    [Sqaml.Database.select_rows] raises a custom Failure when the table does
    not exist. *)
let test_select_rows_nonexistent_table =
  as_test "test_select_rows_nonexistent_table" (fun () ->
      drop_tables ();
      let select_rows () =
        Sqaml.Database.select_rows "nonexistent" [ "example" ]
          (fun _ -> true)
          ""
      in
      assert_raises (Failure "Table does not exist") select_rows)

(** [test_print_value] is an OUnit test that checks that [Sqaml.Row.print_value]
    prints the correct representation of a value, for all value types. *)
let test_print_value =
  as_test "test_print_value" (fun () ->
      let output =
        with_redirected_stdout (fun () -> Sqaml.Row.print_value (Int 5))
        ^ with_redirected_stdout (fun () -> Sqaml.Row.print_value (Float 4.5))
        ^ with_redirected_stdout (fun () ->
              Sqaml.Row.print_value (Varchar "hello"))
        ^ with_redirected_stdout (fun () -> Sqaml.Row.print_value Null)
        ^ with_redirected_stdout (fun () ->
              Sqaml.Row.print_value (Date "2022-12-12"))
      in
      assert_equal ~printer:printer_wrapper "5\n4.5\nhello\nnull\n2022-12-12\n"
        output)

(** [test_value_equals] is an OUnit test that checks that [Sqaml.Row.value_equals]
    returns the correct boolean value when comparing two values, for all supported value types. *)
let test_value_equals =
  as_test "test_value_equals" (fun () ->
      assert_equal (Sqaml.Row.value_equals (Int 1) (Int 1)) true;
      assert_equal
        (Sqaml.Row.value_equals (Varchar "test") (Varchar "test"))
        true;
      assert_equal (Sqaml.Row.value_equals (Float 1.0) (Float 1.0)) true;
      assert_equal
        (Sqaml.Row.value_equals (Date "2022-01-01") (Date "2022-01-01"))
        true;
      assert_equal (Sqaml.Row.value_equals (Int 1) (Int 2)) false;
      assert_equal (Sqaml.Row.value_equals (Int 1) (Varchar "griffin")) false;
      assert_equal (Sqaml.Row.value_not_equals (Int 1) (Int 2)) true)

(** [test_value_less_than] is an OUnit test that checks that [Sqaml.Row.value_less_than]
    returns the correct boolean value when comparing two values, for all supported value types. *)
let test_value_less_than =
  as_test "test_value_less_than" (fun () ->
      assert_equal (Sqaml.Row.value_less_than (Int 1) (Int 2)) true;
      assert_equal (Sqaml.Row.value_less_than (Int 2) (Int 1)) false;
      assert_equal (Sqaml.Row.value_less_than (Int 1) (Int 1)) false;
      assert_equal (Sqaml.Row.value_less_than (Float 1.0) (Float 2.0)) true;
      assert_equal (Sqaml.Row.value_less_than (Float 2.0) (Float 1.0)) false;
      assert_equal (Sqaml.Row.value_less_than (Float 1.0) (Float 1.0)) false;
      assert_equal
        (Sqaml.Row.value_less_than (Varchar "2022-01-01") (Varchar "2022-01-02"))
        true;
      assert_equal
        (Sqaml.Row.value_less_than (Date "2022-01-01") (Varchar "2022-01-02"))
        false;
      assert_equal
        (Sqaml.Row.value_less_than (Date "2022-01-01") (Date "2022-01-02"))
        true;
      assert_equal
        (Sqaml.Row.value_less_than (Date "2022-01-02") (Date "2022-01-01"))
        false;
      assert_equal
        (Sqaml.Row.value_less_than (Date "2022-01-01") (Date "2022-01-01"))
        false)

(** [test_value_greater_than] is an OUnit test that checks that [Sqaml.Row.value_greater_than]
    returns the correct boolean value when comparing two values, for all supported value types. *)
let test_value_greater_than =
  as_test "test_value_greater_than" (fun () ->
      assert_equal (Sqaml.Row.value_greater_than (Int 1) (Int 2)) false;
      assert_equal (Sqaml.Row.value_greater_than (Int 2) (Int 1)) true;
      assert_equal (Sqaml.Row.value_greater_than (Int 1) (Int 1)) false;
      assert_equal (Sqaml.Row.value_greater_than (Float 1.0) (Float 2.0)) false;
      assert_equal (Sqaml.Row.value_greater_than (Float 2.0) (Float 1.0)) true;
      assert_equal (Sqaml.Row.value_greater_than (Float 1.0) (Float 1.0)) false;
      assert_equal
        (Sqaml.Row.value_greater_than (Varchar "2022-01-01")
           (Varchar "2022-01-02"))
        false;
      assert_equal
        (Sqaml.Row.value_greater_than (Date "2022-01-01") (Varchar "2022-01-02"))
        false;
      assert_equal
        (Sqaml.Row.value_greater_than (Date "2022-01-01") (Date "2022-01-02"))
        false;
      assert_equal
        (Sqaml.Row.value_greater_than (Date "2022-01-02") (Date "2022-01-01"))
        true;
      assert_equal
        (Sqaml.Row.value_greater_than (Date "2022-01-01") (Date "2022-01-01"))
        false)

(** [test_tokenize_query] ensures that our tokenizer of strings can successfully
    convert said strings into tokens for use by the parser. *)
let test_tokenize_query =
  as_test "test_tokenize_query" (fun () ->
      assert_equal
        [ Sqaml.Parser.IntKeyword ]
        (Sqaml.Parser.tokenize_query "INT");
      assert_equal
        [ Sqaml.Parser.VarcharKeyword ]
        (Sqaml.Parser.tokenize_query "VARCHAR");
      assert_equal
        [ Sqaml.Parser.PrimaryKey ]
        (Sqaml.Parser.tokenize_query "PRIMARY");
      assert_equal
        [ Sqaml.Parser.PrimaryKey ]
        (Sqaml.Parser.tokenize_query "KEY");
      assert_equal
        [ Sqaml.Parser.Identifier "WHERE" ]
        (Sqaml.Parser.tokenize_query "WHERE");
      assert_equal
        [ Sqaml.Parser.Identifier "TABLE" ]
        (Sqaml.Parser.tokenize_query "TABLE");
      assert_equal
        [ Sqaml.Parser.Identifier "TABLES" ]
        (Sqaml.Parser.tokenize_query "TABLES");
      assert_equal
        [ Sqaml.Parser.Identifier "CREATE" ]
        (Sqaml.Parser.tokenize_query "CREATE");
      assert_equal
        [ Sqaml.Parser.Identifier "INSERT" ]
        (Sqaml.Parser.tokenize_query "INSERT");
      assert_equal
        [ Sqaml.Parser.Identifier "INTO" ]
        (Sqaml.Parser.tokenize_query "INTO");
      assert_equal
        [ Sqaml.Parser.Identifier "SELECT" ]
        (Sqaml.Parser.tokenize_query "SELECT");
      assert_equal
        [ Sqaml.Parser.Identifier "SHOW" ]
        (Sqaml.Parser.tokenize_query "SHOW");
      assert_equal
        [ Sqaml.Parser.Identifier "DROP" ]
        (Sqaml.Parser.tokenize_query "DROP");
      assert_equal
        [ Sqaml.Parser.Identifier "other" ]
        (Sqaml.Parser.tokenize_query "other"))

(** [test_print_tokenized] is an OUnit test that checks that
    [Sqaml.Parser.print_tokenized] prints the correct representation of a list
    of tokens. *)
let test_print_tokenized =
  as_test "test_print_tokenized" (fun () ->
      let output =
        with_redirected_stdout (fun () ->
            Sqaml.Parser.print_tokenized
              [
                Sqaml.Parser.IntKeyword;
                Sqaml.Parser.VarcharKeyword;
                Sqaml.Parser.PrimaryKey;
                Sqaml.Parser.Identifier "WHERE";
              ])
      in
      assert_equal "IntKeyword\nVarcharKeyword\nPrimaryKey\nIdentifier: WHERE\n"
        output)

(** [test_create_table_tokens] is an OUnit test that checks that
    [Sqaml.Parser.tokenize_query] correctly tokenizes a CREATE TABLE query. *)
let test_create_table_tokens =
  as_test "test_create_table_tokens" (fun () ->
      let tokens =
        Sqaml.Parser.tokenize_query
          "CREATE TABLE test_table (example INT PRIMARY KEY);"
      in
      assert_equal tokens
        [
          Sqaml.Parser.Identifier "CREATE";
          Sqaml.Parser.Identifier "TABLE";
          Sqaml.Parser.Identifier "test_table";
          Sqaml.Parser.Identifier "(example";
          Sqaml.Parser.IntKeyword;
          Sqaml.Parser.PrimaryKey;
          Sqaml.Parser.Identifier "KEY);";
        ])
let test_select_with_order =
  as_test "test_select_with_order" (fun () ->
      create_tables ();
      Sqaml.Database.insert_row "test_table"
        [ "example"; "example2"; "example3"; "example4" ]
        [ "0"; "2022-12-12"; "4.5"; "null" ];
      Sqaml.Database.insert_row "test_table"
        [ "example"; "example2"; "example3"; "example4" ]
        [ "1"; "2022-12-12"; "4.5"; "null" ];
      Sqaml.Database.insert_row "test_table"
        [ "example"; "example2"; "example3"; "example4" ]
        [ "2"; "2022-12-12"; "4.5"; "null" ];
      let output =
        with_redirected_stdout (fun () ->
            Sqaml.Parser.parse_and_execute_query
              "SELECT example, example2, example3, example4 FROM test_table ORDER BY example DESC")
      in
      assert_equal ~printer:printer_wrapper "2 2022-12-12 4.500000 NULL \n1 2022-12-12 4.500000 NULL \n0 2022-12-12 4.500000 NULL \n"
        output;
      drop_tables ())

(** [test_compare_row] is an OUnit test that checks that [Sqaml.Table.compare_row]
    returns the correct integer value when comparing two rows. *)
let test_compare_row =
  as_test "test_compare_row" (fun () ->
      let row1 : Sqaml.Row.row = { values = [ Int 1; Int 2; Int 3 ] } in
      let row2 : Sqaml.Row.row = { values = [ Int 1; Int 3; Int 2 ] } in
      assert_equal 0 (Sqaml.Table.compare_row 0 row1 row2);
      assert_equal (-1) (Sqaml.Table.compare_row 1 row1 row2);
      assert_equal 1 (Sqaml.Table.compare_row 2 row1 row2))

(** [test_parse_and_execute_query] is a huge list of assertions that
    verifies the functionality of 90+% of all possible SQL queries or failed inputs.*)
let test_parse_and_execute_query =
  as_test "test_parse_and_execute_query" (fun () ->
      drop_tables ();
      let output_create =
        with_redirected_stdout (fun () ->
            Sqaml.Parser.parse_and_execute_query
              "CREATE TABLE users (id INT PRIMARY KEY, name VARCHAR, age INT)")
      in
      assert_equal ~printer:printer_wrapper "id: int\nname: varchar\nage: int\n"
        output_create;
      let output_create2 =
        with_redirected_stdout (fun () ->
            Sqaml.Parser.parse_and_execute_query
              "CREATE TABLE another (auto PRIMARY KEY)")
      in
      assert_equal ~printer:printer_wrapper "auto: int\n" output_create2;

      Sqaml.Parser.parse_and_execute_query "DROP TABLE another";

      let output_insert =
        with_redirected_stdout (fun () ->
            Sqaml.Parser.parse_and_execute_query
              "INSERT INTO users (id, name, age) VALUES (1, 'Simon', 25)")
      in
      assert_equal ~printer:printer_wrapper
        "id: int\nname: varchar\nage: int\n1 'Simon' 25"
        (String.trim output_insert);

      let output_show =
        with_redirected_stdout (fun () ->
            Sqaml.Parser.parse_and_execute_query
              "SHOW COLUMNS FROM users")
      in
      assert_equal ~printer:printer_wrapper
        "id : Integer |name : Varchar |age : Integer |"
        (String.trim output_show);

      let output_select =
        with_redirected_stdout (fun () ->
            Sqaml.Parser.parse_and_execute_query "SELECT * FROM users")
      in
      assert_equal ~printer:printer_wrapper "1 'Simon' 25"
        (String.trim output_select);

      let output_update =
        with_redirected_stdout (fun () ->
            Sqaml.Parser.parse_and_execute_query
              "UPDATE users SET name = 'Clarkson' WHERE id = 1")
      in
      assert_equal ~printer:printer_wrapper "" output_update;
      let output_select_updated =
        with_redirected_stdout (fun () ->
            Sqaml.Parser.parse_and_execute_query "SELECT * FROM users")
      in
      assert_equal ~printer:printer_wrapper "1 'Clarkson' 25"
        (String.trim output_select_updated);

      let output_delete =
        with_redirected_stdout (fun () ->
            Sqaml.Parser.parse_and_execute_query
              "DELETE FROM users WHERE id = 1 AND name = 'Clarkson'")
      in
      assert_equal ~printer:printer_wrapper "" output_delete;
      let output_select_deleted =
        with_redirected_stdout (fun () ->
            Sqaml.Parser.parse_and_execute_query "SELECT * FROM users")
      in
      assert_equal ~printer:printer_wrapper "" output_select_deleted;
      let output_delete_all =
        with_redirected_stdout (fun () ->
            Sqaml.Parser.parse_and_execute_query "DELETE FROM users")
      in
      assert_equal ~printer:printer_wrapper "" output_delete_all;

      let output_drop =
        with_redirected_stdout (fun () ->
            Sqaml.Parser.parse_and_execute_query "DROP TABLE users")
      in
      assert_equal ~printer:printer_wrapper "" output_drop;
      let output_show =
        with_redirected_stdout (fun () ->
            Sqaml.Parser.parse_and_execute_query "SHOW TABLES")
      in
      assert_equal ~printer:printer_wrapper "No tables in database.\n"
        output_show;
      assert_raises (Failure "Syntax error in column definition") (fun () ->
          Sqaml.Parser.parse_and_execute_query "INSERT INTO 12144");
      assert_raises (Failure "Table must have a primary key") (fun () ->
          Sqaml.Parser.parse_and_execute_query "CREATE TABLE joker");
      assert_raises (Failure "Syntax error in column definition") (fun () ->
          Sqaml.Parser.parse_and_execute_query "CREATE TABLE joker id");
      assert_raises (Failure "Unrecognized update transform clause format.")
        (fun () ->
          Sqaml.Parser.parse_and_execute_query
            "UPDATE users SET name='GLORY' WHERE id=1");
      assert_raises (Failure "Table does not exist") (fun () ->
          Sqaml.Parser.parse_and_execute_query "UPDATE users SET name = 'GLORY'");
      assert_raises (Failure "hd") (fun () ->
          Sqaml.Parser.parse_and_execute_query "SELECT JOKE FROM");
      assert_raises (Failure "Unrecognized where clause format.") (fun () ->
          Sqaml.Parser.parse_and_execute_query
            "create table users (name primary key)";
          Sqaml.Parser.parse_and_execute_query "UPDATE users SET name = 1 WHERE");
      Sqaml.Parser.parse_and_execute_query "DROP TABLE users";
      (* note missing query support for float, date, and null *)
      assert_raises (Failure "Unsupported query") (fun () ->
          Sqaml.Parser.parse_and_execute_query "GOID"))

(** [suite] is the test suite for the SQamL module. *)
let suite =
  "sqaml test suite"
  >::: [
         test_show_all_tables_with_no_tables;
         test_show_all_tables_with_some_tables;
         test_get_column_type_column_present;
         test_get_column_type_table_absent;
         test_construct_transform_table_absent;
         test_get_column_type_column_absent;
         test_construct_transform_column_present;
         test_construct_predicate_column_present;
         test_construct_predicate_table_absent;
         test_insert_row_table_exists;
         test_insert_row_table_absent;
         test_create_table_already_exists;
         test_delete_rows_nonexistent_table;
         test_update_rows_nonexistent_table;
         test_normal_update_rows;
         test_missing_select_all_table;
         test_print_table;
         test_print_nonexistent_table;
         test_select_rows;
         test_select_rows_nonexistent_table;
         test_print_value;
         test_value_equals;
         test_value_less_than;
         test_value_greater_than;
         test_tokenize_query;
         test_print_tokenized;
         test_create_table_tokens;
         test_parse_and_execute_query;
         test_compare_row;
         test_select_with_order;
       ]

let () = run_test_tt_main suite
