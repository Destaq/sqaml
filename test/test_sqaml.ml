open OUnit2

(* TODO: add some docs *)

let printer_wrapper s = s

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

let with_tables f =
  (* Create the tables *)
  Sqaml.Database.create_table
    [
      { name = "example"; col_type = Sqaml.Table.Int_type; primary_key = true };
    ]
    "test_table";
  Sqaml.Database.create_table
    [ { name = "hi"; col_type = Sqaml.Table.Float_type; primary_key = true } ]
    "another_table";

  (* Execute the provided function, ensuring that the tables are dropped afterwards *)
  f ();

  (* Drop the tables *)
  Sqaml.Database.drop_table "test_table";
  Sqaml.Database.drop_table "another_table";
  ()

let as_test name f = name >:: fun _ -> f ()

let with_no_tables f =
  Sqaml.Database.drop_table "test_table";
  Sqaml.Database.drop_table "another_table";
  f ();
  ()

let test_show_all_tables_with_no_tables =
  as_test "test_show_all_tables_with_no_tables" (fun () ->
      with_no_tables (fun () ->
          (* TODO: fix sometimes shows up as empty? *)
          let output = with_redirected_stdout Sqaml.Database.show_all_tables in
          assert_equal ~printer:printer_wrapper "No tables in database.\n"
            output))

let test_show_all_tables_with_some_tables =
  as_test "test_show_all_tables_with_some_tables" (fun () ->
      with_tables (fun () ->
          let output = with_redirected_stdout Sqaml.Database.show_all_tables in
          assert_equal ~printer:printer_wrapper
            "Tables:\nanother_table\ntest_table\n" output))

let test_get_column_type_column_present =
  as_test "test_get_column_type_column_present" (fun () ->
      with_tables (fun () ->
          let output = Sqaml.Database.get_column_type "test_table" "example" in
          assert_equal output Sqaml.Table.Int_type))

let test_get_column_type_table_absent =
  as_test "test_get_column_type_table_absent" (fun () ->
      with_tables (fun () ->
          let failure_fun () =
            Sqaml.Database.get_column_type "no_table" "nonexistent"
          in
          OUnit2.assert_raises (Failure "Table does not exist") failure_fun))

let test_get_column_type_column_absent =
  as_test "test_get_column_type_column_absent" (fun () ->
      with_tables (fun () ->
          try
            let _ = Sqaml.Database.get_column_type "test_table" "nonexistent" in
            assert_failure
              "Expected failure for nonexistent column, but got none."
          with
          | Failure msg ->
              assert_equal ~printer:printer_wrapper "Column not found." msg
          | _ ->
              assert_failure
                "Expected Failure exception, but got different exception."))

let test_construct_transform_column_present =
  as_test "test_construct_transform_column_present" (fun () ->
      with_tables (fun () ->
          let updated_row =
            Sqaml.Database.construct_transform [ "example" ] [ Int 1 ]
              "test_table" { values = [ Int 0 ] }
          in
          assert_equal updated_row.values [ Int 1 ]))

let test_construct_transform_table_absent =
  as_test "test_construct_transform_table_absent" (fun () ->
      let updated_row () =
        Sqaml.Database.construct_transform [ "example" ] [ Int 1 ] "test_table"
          { values = [ Int 0 ] }
      in
      assert_raises (Failure "Table does not exist") updated_row)

let test_construct_predicate_column_present =
  as_test "test_construct_predicate_column_present" (fun () ->
      with_tables (fun () ->
          let predicate =
            Sqaml.Database.construct_predicate [ "example" ] [ Int 1 ]
              [ (fun (x : Sqaml.Row.value) (y : Sqaml.Row.value) -> x > y) ]
              "test_table"
          in
          let result = predicate { values = [ Int 0 ] } in
          assert_equal result false))

let test_construct_predicate_table_absent =
  as_test "test_construct_predicate_table_absent" (fun () ->
      with_no_tables (fun () ->
          let predicate =
            Sqaml.Database.construct_predicate [ "example" ] [ Int 1 ]
              [ (fun (x : Sqaml.Row.value) (y : Sqaml.Row.value) -> x > y) ]
              "asdfsdfsdf"
          in
          assert_raises (Failure "Table does not exist") (fun () ->
              predicate { values = [ Int 0 ] })))

let test_insert_row_table_exists =
  as_test "test_insert_row_table_exists" (fun () ->
      with_tables (fun () ->
          let values = [ "5" ] in
          let output =
            with_redirected_stdout (fun () ->
                Sqaml.Database.insert_row "test_table" [ "example" ] values;
                Sqaml.Database.delete_rows "test_table" (fun _ -> true))
          in
          assert_equal ~printer:printer_wrapper "example: int\n5 \n" output))

let test_insert_row_table_absent =
  as_test "test_insert_row_table_absent" (fun () ->
      let values = [ "12" ] in
      let insert_absent_table () =
        Sqaml.Database.insert_row "test_table" [ "example" ] values
      in
      assert_raises (Failure "Table does not exist") insert_absent_table)

let test_create_table_already_exists =
  as_test "test_create_table_already_exists" (fun () ->
      with_tables (fun () ->
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
          assert_raises (Failure "Table already exists") create_table))

let test_delete_rows_nonexistent_table =
  as_test "test_delete_rows_nonexistent_table" (fun () ->
      with_no_tables (fun () ->
          let delete_rows () =
            Sqaml.Database.delete_rows "nonexistent" (fun _ -> true)
          in
          assert_raises (Failure "Table does not exist") delete_rows))

let test_update_rows_nonexistent_table =
  as_test "test_update_rows_nonexistent_table" (fun () ->
      with_no_tables (fun () ->
          let update () =
            Sqaml.Database.update_rows "example"
              (fun _ -> true)
              (fun _ -> { values = [ Int 1 ] })
          in
          assert_raises (Failure "Table does not exist") update))

let test_normal_update_rows =
  as_test "test_normal_update_rows" (fun () ->
      with_tables (fun () ->
          Sqaml.Database.insert_row "test_table" [ "example" ] [ "0" ];
          let output =
            with_redirected_stdout (fun () ->
                Sqaml.Database.update_rows "test_table"
                  (fun row -> row.values = [ Int 0 ])
                  (fun _ -> { values = [ Int 1 ] });
                Sqaml.Database.select_all "test_table")
          in
          assert_equal ~printer:printer_wrapper "1 \n" output))

let test_missing_select_all_table =
  as_test "test_missing_select_all_table" (fun () ->
      with_no_tables (fun () ->
          let select_all () = Sqaml.Database.select_all "nonexistent" in
          assert_raises (Failure "Table does not exist") select_all))

let test_print_table =
  as_test "test_normal_update_rows" (fun () ->
      with_tables (fun () ->
          Sqaml.Database.insert_row "test_table" [ "example" ] [ "0" ];
          let output =
            with_redirected_stdout (fun () ->
                Sqaml.Database.update_rows "test_table"
                  (fun row -> row.values = [ Int 0 ])
                  (fun _ -> { values = [ Int 1 ] });
                Sqaml.Database.print_table "test_table")
          in
          assert_equal ~printer:printer_wrapper "example: int\n1 \n" output))

let test_print_nonexistent_table =
  as_test "test_print_nonexistent_table" (fun () ->
      with_no_tables (fun () ->
          let print_table () = Sqaml.Database.print_table "nonexistent" in
          assert_raises (Failure "Table does not exist") print_table))

let test_select_rows =
  as_test "test_select_rows" (fun () ->
      with_tables (fun () ->
          Sqaml.Database.insert_row "test_table" [ "example" ] [ "0" ];
          assert_equal
            (Sqaml.Database.select_rows "test_table" [ "example" ] (fun _ ->
                 true))
            [ { values = [ Int 0 ] } ]))

let test_select_rows_nonexistent_table =
  as_test "test_select_rows_nonexistent_table" (fun () ->
      with_no_tables (fun () ->
          let select_rows () =
            Sqaml.Database.select_rows "nonexistent" [ "example" ] (fun _ ->
                true)
          in
          assert_raises (Failure "Table does not exist") select_rows))

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
      assert_equal (Sqaml.Row.value_equals (Int 1) (Int 2)) false)

let test_value_less_than =
  as_test "test_value_less_than" (fun () ->
      assert_equal (Sqaml.Row.value_less_than (Int 1) (Int 2)) true;
      assert_equal (Sqaml.Row.value_less_than (Int 2) (Int 1)) false;
      assert_equal (Sqaml.Row.value_less_than (Int 1) (Int 1)) false;
      assert_equal (Sqaml.Row.value_less_than (Float 1.0) (Float 2.0)) true;
      assert_equal (Sqaml.Row.value_less_than (Float 2.0) (Float 1.0)) false;
      assert_equal (Sqaml.Row.value_less_than (Float 1.0) (Float 1.0)) false;
      assert_equal
        (Sqaml.Row.value_less_than (Date "2022-01-01") (Date "2022-01-02"))
        true;
      assert_equal
        (Sqaml.Row.value_less_than (Date "2022-01-02") (Date "2022-01-01"))
        false;
      assert_equal
        (Sqaml.Row.value_less_than (Date "2022-01-01") (Date "2022-01-01"))
        false)

let test_value_greater_than =
  as_test "test_value_greater_than" (fun () ->
      assert_equal (Sqaml.Row.value_greater_than (Int 1) (Int 2)) false;
      assert_equal (Sqaml.Row.value_greater_than (Int 2) (Int 1)) true;
      assert_equal (Sqaml.Row.value_greater_than (Int 1) (Int 1)) false;
      assert_equal (Sqaml.Row.value_greater_than (Float 1.0) (Float 2.0)) false;
      assert_equal (Sqaml.Row.value_greater_than (Float 2.0) (Float 1.0)) true;
      assert_equal (Sqaml.Row.value_greater_than (Float 1.0) (Float 1.0)) false;
      assert_equal
        (Sqaml.Row.value_greater_than (Date "2022-01-01") (Date "2022-01-02"))
        false;
      assert_equal
        (Sqaml.Row.value_greater_than (Date "2022-01-02") (Date "2022-01-01"))
        true;
      assert_equal
        (Sqaml.Row.value_greater_than (Date "2022-01-01") (Date "2022-01-01"))
        false)

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
       ]

let () = run_test_tt_main suite
