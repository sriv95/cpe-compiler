open A2_parsing
open Typechecker

let typecheck_string (s : string) : bool =
  match parse s with
  | None -> print_string (s ^ ": syntax error\n"); false
  | Some prog -> let tc_res = typecheck prog in
    print_string (s ^ ": typechecking " ^ (if tc_res then "passed" else "failed"));
    print_newline ();
    tc_res

let typecheck_file (s : string) : bool =
  match parse_file s with
  | None -> print_string (s ^ ": syntax error\n"); false
  | Some prog -> let tc_res = typecheck prog in
    print_string (s ^ ": typechecking " ^ (if tc_res then "passed" else "failed"));
    print_newline ();
    tc_res

let rec repl_loop () =
  print_string "enter file name: ";
  let filename = read_line () in
  if String.length filename == 0 then ()
  else (
    let _ = typecheck_file filename in
    repl_loop ()
  )