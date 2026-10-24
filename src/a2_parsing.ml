open Utils
open Lexer
open Ast
open S_expr

let print_lexbuf_position outx (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s" (string_of_position pos)

let parse_with_error lexbuf : unit prog option =
  try Some (Parser.start Lexer.read lexbuf) with
  | SyntaxError (msg, pos) ->
    Printf.fprintf stderr "%s: %s\n" (string_of_position pos) msg;
    None
  | Parser.Error ->
    Printf.fprintf stderr "syntax error at %a: %s\n"
        print_lexbuf_position lexbuf (Lexing.lexeme lexbuf);
    None

let parse (s : string) : unit prog option =
  let lexbuf = Lexing.from_string s in
  parse_with_error lexbuf

let pretty_print_s_expr s =
  string_of_s_expr (S_parser.start S_lexer.read (Lexing.from_string s))

let pretty_parse (s : string) : string =
  let lexbuf = Lexing.from_string s in
  match parse_with_error lexbuf with
  | None -> "syntax error"
  | Some prog -> pretty_print_s_expr (string_of_prog prog)

let parse_file (filename : string) : unit prog option =
  let ic = open_in filename in
  try
    let lexbuf = Lexing.from_channel ic in
    let _ = Lexing.set_filename lexbuf filename
    and prog = parse_with_error lexbuf in
    close_in ic;
    prog
  with e ->
    (* some unexpected exception occurs *)
    close_in_noerr ic;
    (* emergency closing *)
    raise e

let parse_test (s : string) : unit =
  let filedir = Filename.dirname s
  and filename = Filename.basename s in
  let outdir = "out/02-parsing/" ^ filedir in
  create_directory outdir;
  let outfile = outdir ^ Filename.dir_sep ^ filename in
  let oc = open_out outfile in
  match parse_file s with
  | None -> Printf.fprintf oc "%s\n" "syntax error"
  | Some prog -> Printf.fprintf oc "%s\n"
      (pretty_print_s_expr (string_of_prog prog));
  close_out oc;
  print_string ("output written to " ^ outfile);
  print_newline ()

let rec repl_loop () =
  print_string "enter file name: ";
  let filename = read_line () in
  if String.length filename == 0 then ()
  else (
    parse_test filename;
    repl_loop ()
  )