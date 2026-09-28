let () =
  Array.iter
    (fun f ->
      CpecmuCompiler.A1_lexing.lex_test f;
      List.iter print_endline (CpecmuCompiler.A1_lexing.pretty_lex_file f))
    (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
