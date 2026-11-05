open Utils
open A2_parsing
open Typechecker

(* imports resolve to <name>.cpe in the same directory as the primary file *)
let loader_for (dir : string) : loader = fun name ->
  let path = Filename.concat dir (name ^ ".cpe") in
  if Sys.file_exists path then parse_file path else None

let report (name : string) (res : (_, string) result) : bool =
  match res with
  | Ok _ ->
      print_string (name ^ ": typechecking passed"); print_newline (); true
  | Error msg ->
      prerr_endline msg;
      print_string (name ^ ": typechecking failed"); print_newline (); false

let typecheck_string (s : string) : bool =
  match parse s with
  | None -> print_string (s ^ ": syntax error\n"); false
  | Some prog -> report s (typecheck_result prog)

let typecheck_file (s : string) : bool =
  match parse_file s with
  | None -> print_string (s ^ ": syntax error\n"); false
  | Some prog ->
      let res = typecheck_result ~load:(loader_for (Filename.dirname s)) prog in
      (* write the outcome where bin/typecheck.sh expects it *)
      if Filename.is_relative s then begin
        let outdir = "out/03-typechecking/" ^ Filename.dirname s in
        create_directory outdir;
        let oc = open_out ("out/03-typechecking/" ^ s) in
        (match res with
         | Ok _ -> output_string oc "typechecking passed\n"
         | Error msg -> output_string oc ("typechecking failed\n" ^ msg ^ "\n"));
        close_out oc
      end;
      report s res

let rec repl_loop () =
  print_string "enter file name: ";
  let filename = read_line () in
  if String.length filename == 0 then ()
  else (
    let _ = typecheck_file filename in
    repl_loop ()
  )
