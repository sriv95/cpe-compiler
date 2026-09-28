open OUnit2
open CpecmuCompiler.A1_lexing
open CpecmuCompiler.Token

let tests = "sample test suite for lexing" >::: [
  (* Original sample tests *)
  "" >:: (fun _ -> assert_equal [EOF] (lex ""));
  "+" >:: (fun _ -> assert_equal [Symbol("+"); EOF] (lex "+"));
  "+ +" >:: (fun _ -> assert_equal [Symbol("+"); Symbol("+"); EOF] (lex "+ +"));
  (* the following tests fail initially *)
  "++" >:: (fun _ -> assert_equal [Symbol("++"); EOF] (lex "++"));
  "'\\n'" >:: (fun _ -> assert_equal [CharLit('\n'); EOF] (lex "'\\n'"));
  "\"hello\"" >:: (fun _ -> assert_equal [StringLit("hello"); EOF] (lex "\"hello\""));

  (* Keywords *)
  "keywords" >:: (fun _ -> assert_equal [
    Keyword("def"); Keyword("import"); Keyword("as"); Keyword("match");
    Keyword("with"); Keyword("if"); Keyword("then"); Keyword("else");
    Keyword("let"); Keyword("in"); Keyword("and"); Keyword("data");
    Keyword("type"); EOF
  ] (lex "def import as match with if then else let in and data type"));

  (* Identifiers: VariableName & TypeOrConstructorName & Wildcard *)
  "identifiers" >:: (fun _ -> assert_equal [
    VariableName("x"); VariableName("map"); VariableName("isEven");
    TypeOrConstructorName("Integer"); TypeOrConstructorName("Leaf");
    TypeOrConstructorName("Tree"); VariableName("_"); EOF
  ] (lex "x map isEven Integer Leaf Tree _"));

  (* BoolLit *)
  "bool_literals" >:: (fun _ -> assert_equal [
    BoolLit(true); BoolLit(false); EOF
  ] (lex "True False"));

  (* IntLit *)
  "int_literals" >:: (fun _ -> assert_equal [
    IntLit(0); IntLit(42); IntLit(7); EOF
  ] (lex "0 42 007"));

  (* FloatLit *)
  "float_literals" >:: (fun _ -> assert_equal [
    FloatLit(0.0); FloatLit(3.14); FloatLit(1e10); FloatLit(0.0025); EOF
  ] (lex "0.0 3.14 1e10 2.5e-3"));

  (* CharLit & Escape Sequences *)
  "char_literals" >:: (fun _ -> assert_equal [
    CharLit('a'); CharLit('\\'); CharLit('\''); CharLit('"');
    CharLit('\n'); CharLit('\r'); CharLit('\t'); CharLit('\b'); EOF
  ] (lex "'a' '\\\\' '\\'' '\\\"' '\\n' '\\r' '\\t' '\\b'"));

  (* StringLit & Escape Sequences *)
  "string_literals" >:: (fun _ -> assert_equal [
    StringLit(""); StringLit("hello"); StringLit("line1\nline2\ttab");
    StringLit("escaped \" quote"); EOF
  ] (lex "\"\" \"hello\" \"line1\\nline2\\ttab\" \"escaped \\\" quote\""));

  (* Single-character Symbols *)
  "single_char_symbols" >:: (fun _ -> assert_equal [
    Symbol("+"); Symbol("-"); Symbol("*"); Symbol("/"); Symbol("%");
    Symbol("<"); Symbol(">"); Symbol("="); Symbol("|"); Symbol("\\");
    Symbol(":"); Symbol("!"); Symbol("."); Symbol("$"); Symbol(",");
    Symbol("("); Symbol(")"); Symbol("["); Symbol("]"); EOF
  ] (lex "+ - * / % < > = | \\ : ! . $ , ( ) [ ]"));

  (* Multi-character Symbols *)
  "multi_char_symbols" >:: (fun _ -> assert_equal [
    Symbol("->"); Symbol("=>"); Symbol("=="); Symbol("!="); Symbol("<=");
    Symbol(">="); Symbol("::"); Symbol("++"); Symbol("//"); Symbol("&&");
    Symbol("||"); EOF
  ] (lex "-> => == != <= >= :: ++ // && ||"));

  (* Comments *)
  "single_line_comment" >:: (fun _ -> assert_equal [
    IntLit(1); IntLit(2); EOF
  ] (lex "1 # this is a comment\n 2"));

  "multi_line_comment" >:: (fun _ -> assert_equal [
    IntLit(1); IntLit(2); EOF
  ] (lex "1 (* this is a \n multi-line comment *) 2"));

  (* Error tokens *)
  "err_invalid_char_escape" >:: (fun _ ->
    match lex "'\\k'" with
    | [Error ("'\\k'", _)] -> ()
    | _ -> assert_failure "expected Error token for invalid char escape"
  );

  "err_unclosed_string" >:: (fun _ ->
    match lex "\"unclosed string" with
    | [Error ("unclosed string", _)] -> ()
    | _ -> assert_failure "expected Error token for unclosed string"
  );

  "err_nested_comment" >:: (fun _ ->
    match lex "(* (* nested *) *)" with
    | [Error ("(*", _)] -> ()
    | _ -> assert_failure "expected Error token for nested comment"
  )
]

let _ = run_test_tt_main tests