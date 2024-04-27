open OUnit2

(* TODO: add some docs *)

let printer_wrapper s = s

let with_redirected_stdout f =
  let original_stdout = Unix.dup Unix.stdout in
  let temp_out = open_out "temp_out" in
  Unix.dup2 (Unix.descr_of_out_channel temp_out) Unix.stdout;
  f ();
  flush stdout;
  Unix.dup2 original_stdout Unix.stdout;
  close_out temp_out;
  let temp_in = open_in "temp_out" in
  let output = input_line temp_in in
  output

let as_test name f = name >:: fun _ -> f ()
let print_hello () = print_string "hello"

let test_print_hello =
  as_test "test_print_hello" (fun () ->
      let output = with_redirected_stdout print_hello in
      assert_equal ~printer:printer_wrapper "hello" output)

let suite = "sqaml test suite" >::: [ test_print_hello ]
let () = run_test_tt_main suite
