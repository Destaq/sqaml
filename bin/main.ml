open Sqaml.Parser
open Sqaml.Storage

(**Main program driver to retrieve user input and call backend commands accordingly.*)
let rec main_loop () =
  print_string "Enter an SQL command (or 'exit;' to quit): ";
  let rec read_lines acc =
    let line = read_line () in
    if String.contains line ';' then
      String.sub line 0 (String.index line ';') :: acc
    else read_lines (line :: acc)
  in
  let query = String.concat " " (List.rev (read_lines [])) in
  match query with
  | "exit" -> sync_on_exit ()
  | _ -> (
      try
        parse_and_execute_query query;
        main_loop ()
      with Failure msg ->
        print_endline ("Error: " ^ msg);
        main_loop ())

(**Generate the Camel start screen for SQaml (note: this is the most important part of the project).*)
let () =
  let orange = "\027[38;5;208m" in
  let reset = "\027[0m" in

  let ascii_art =
    orange
    ^ "  _oo\\\n\
      \  (__/ \\  _  _\n\
      \    \\  \\/ \\/ \\\n\
      \    (         )\\\n\
      \      \\_______/  \\\n\
      \      [[] [[]]\n\
      \      [[] [[]]" ^ reset
  in
  print_endline ascii_art
;;

load_from_storage ();
print_endline "Welcome to the SQAMLVerse!";
main_loop ();
print_endline "Goodbye!"
