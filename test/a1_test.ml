open OUnit2
open CpecmuCompiler.A1_lexing
open CpecmuCompiler.Token

let tests = "sample test suite for lexing" >::: [
  (* Original sample tests *)
  "" >:: (fun _ -> assert_equal [EOF] (lex ""));
  "+" >:: (fun _ -> assert_equal [Symbol("+"); EOF] (lex "+"));
  "+ +" >:: (fun _ -> assert_equal [Symbol("+"); Symbol("+"); EOF] (lex "+ +"));
  "++" >:: (fun _ -> assert_equal [Symbol("++"); EOF] (lex "++"));
  "'\\n'" >:: (fun _ -> assert_equal [CharLit('\n'); EOF] (lex "'\\n'"));
  "\"hello\"" >:: (fun _ -> assert_equal [StringLit("hello"); EOF] (lex "\"hello\""));

  (* 01_keywords.cpe *)
  "01_keywords" >:: (fun _ -> assert_equal [
    Keyword("def"); Keyword("import"); Keyword("as"); Keyword("match");
    Keyword("with"); Keyword("if"); Keyword("then"); Keyword("else");
    Keyword("let"); Keyword("in"); Keyword("and"); Keyword("data");
    Keyword("type"); EOF
  ] (lex "def import as match with if then else let in and data type"));

  (* 02_identifiers.cpe *)
  "02_identifiers" >:: (fun _ -> assert_equal [
    VariableName("x"); VariableName("x1"); VariableName("x2"); VariableName("map");
    VariableName("isEven"); VariableName("foldl"); VariableName("accum1");
    VariableName("_var"); VariableName("_");
    TypeOrConstructorName("Integer"); TypeOrConstructorName("Cons1");
    TypeOrConstructorName("String"); TypeOrConstructorName("Bool");
    TypeOrConstructorName("Char"); TypeOrConstructorName("Double");
    TypeOrConstructorName("Tree"); TypeOrConstructorName("Leaf");
    TypeOrConstructorName("Node"); TypeOrConstructorName("ConsIntList"); EOF
  ] (lex "x x1 x2 map isEven foldl accum1 _var _\nInteger Cons1 String Bool Char Double Tree Leaf Node ConsIntList"));

  (* 03_bool_literals.cpe *)
  "03_bool_literals" >:: (fun _ -> assert_equal [
    BoolLit(true); BoolLit(false);
    Keyword("if"); BoolLit(true); Keyword("then"); BoolLit(false); Keyword("else"); BoolLit(true); EOF
  ] (lex "True False\nif True then False else True"));

  (* 04_int_literals.cpe *)
  "04_int_literals" >:: (fun _ -> assert_equal [
    IntLit(0); IntLit(1); IntLit(42); IntLit(100); IntLit(7); IntLit(987654321); EOF
  ] (lex "0 1 42 100 007 987654321"));

  (* 05_float_literals.cpe *)
  "05_float_literals" >:: (fun _ -> assert_equal [
    FloatLit(0.0); FloatLit(3.14159); FloatLit(123.456); FloatLit(1e10);
    FloatLit(0.0025); FloatLit(10000.0); EOF
  ] (lex "0.0 3.14159 123.456 1e10 2.5e-3 0.1E+5"));

  (* 06_char_literals.cpe *)
  "06_char_literals" >:: (fun _ -> assert_equal [
    CharLit('a'); CharLit('Z'); CharLit('0'); CharLit('\\'); CharLit('\'');
    CharLit('"'); CharLit('\n'); CharLit('\r'); CharLit('\t'); CharLit('\b'); EOF
  ] (lex "'a' 'Z' '0' '\\\\' '\\'' '\\\"' '\\n' '\\r' '\\t' '\\b'"));

  (* 07_string_literals.cpe *)
  "07_string_literals" >:: (fun _ -> assert_equal [
    StringLit(""); StringLit("Hello, world!"); StringLit("Line 1\nLine 2\tTabbed");
    StringLit("Escaped quotes: \"hello\" and backslash: \\"); EOF
  ] (lex "\"\"\n\"Hello, world!\"\n\"Line 1\\nLine 2\\tTabbed\"\n\"Escaped quotes: \\\"hello\\\" and backslash: \\\\\""));

  (* 08_single_char_symbols.cpe *)
  "08_single_char_symbols" >:: (fun _ -> assert_equal [
    Symbol("+"); Symbol("-"); Symbol("*"); Symbol("/"); Symbol("%");
    Symbol("<"); Symbol(">"); Symbol("="); Symbol("|"); Symbol("\\");
    Symbol(":"); Symbol("!"); Symbol("."); Symbol("$"); Symbol(",");
    Symbol("("); Symbol(")"); Symbol("["); Symbol("]"); EOF
  ] (lex "+ - * / % < > = | \\ : ! . $ , ( ) [ ]"));

  (* 09_multi_char_symbols.cpe *)
  "09_multi_char_symbols" >:: (fun _ -> assert_equal [
    Symbol("->"); Symbol("=>"); Symbol("=="); Symbol("!="); Symbol("<=");
    Symbol(">="); Symbol("::"); Symbol("++"); Symbol("//"); Symbol("&&");
    Symbol("||"); EOF
  ] (lex "-> => == != <= >= :: ++ // && ||"));

  (* 10_comments_line.cpe *)
  "10_comments_line" >:: (fun _ -> assert_equal [
    Keyword("def"); VariableName("main"); Symbol("="); IntLit(42); EOF
  ] (lex "def main = # starting comment here\n  42 # another comment at the end of line\n# full line comment"));

  (* 11_comments_multiline.cpe *)
  "11_comments_multiline" >:: (fun _ -> assert_equal [
    Keyword("def"); VariableName("x"); Symbol("="); IntLit(10); EOF
  ] (lex "(* This is a\n   multi-line comment *)\ndef x = (* inline multiline comment *) 10"));

  (* 12_hello_world.cpe *)
  "12_hello_world" >:: (fun _ -> assert_equal [
    Keyword("def"); VariableName("main"); Symbol("("); VariableName("arg");
    Symbol(":"); Symbol("["); TypeOrConstructorName("String"); Symbol("]");
    Symbol(")"); Symbol(":"); TypeOrConstructorName("String"); Symbol("=");
    StringLit("Hello, world!"); EOF
  ] (lex "def main (arg : [String]) : String = # ignores input\n  \"Hello, world!\""));

  (* 13_pervasives_functions.cpe *)
  "13_pervasives_functions" >:: (fun _ -> assert_equal [
    Keyword("def"); VariableName("map"); Symbol("("); VariableName("f"); Symbol(":");
    VariableName("a"); Symbol("->"); VariableName("b"); Symbol(")"); Symbol("(");
    VariableName("xs"); Symbol(":"); Symbol("["); VariableName("a"); Symbol("]");
    Symbol(")"); Symbol(":"); Symbol("["); VariableName("b"); Symbol("]"); Symbol("=");
    Keyword("match"); VariableName("xs"); Keyword("with"); Symbol("|"); Symbol("[");
    Symbol("]"); Symbol("->"); Symbol("["); Symbol("]"); Symbol("|"); VariableName("hd");
    Symbol("::"); VariableName("tl"); Symbol("->"); VariableName("f"); VariableName("hd");
    Symbol("::"); VariableName("map"); VariableName("f"); VariableName("tl");

    Keyword("def"); VariableName("filter"); Symbol("("); VariableName("p"); Symbol(":");
    VariableName("a"); Symbol("->"); TypeOrConstructorName("Bool"); Symbol(")");
    Symbol("("); VariableName("xs"); Symbol(":"); Symbol("["); VariableName("a");
    Symbol("]"); Symbol(")"); Symbol(":"); Symbol("["); VariableName("a"); Symbol("]");
    Symbol("="); Keyword("match"); VariableName("xs"); Keyword("with"); Symbol("|");
    Symbol("["); Symbol("]"); Symbol("->"); Symbol("["); Symbol("]"); Symbol("|");
    VariableName("hd"); Symbol("::"); VariableName("tl"); Symbol("->"); Keyword("if");
    VariableName("p"); VariableName("hd"); Keyword("then"); VariableName("hd"); Symbol("::");
    VariableName("filter"); VariableName("p"); VariableName("tl"); Keyword("else");
    VariableName("filter"); VariableName("p"); VariableName("tl"); EOF
  ] (lex "def map (f : a -> b) (xs : [a]) : [b] =\n  match xs with\n  | [] -> []\n  | hd::tl -> f hd :: map f tl\n\ndef filter (p : a -> Bool) (xs : [a]) : [a] =\n  match xs with\n  | [] -> []\n  | hd::tl -> if p hd then hd :: filter p tl else filter p tl"));

  (* 14_data_types.cpe *)
  "14_data_types" >:: (fun _ -> assert_equal [
    Keyword("data"); TypeOrConstructorName("Tree"); VariableName("a"); Symbol("=");
    TypeOrConstructorName("Leaf"); Symbol("|"); TypeOrConstructorName("Node"); VariableName("a");
    Symbol("("); TypeOrConstructorName("Tree"); VariableName("a"); Symbol(")"); Symbol("(");
    TypeOrConstructorName("Tree"); VariableName("a"); Symbol(")");
    Keyword("data"); TypeOrConstructorName("Either"); VariableName("a"); VariableName("b");
    Symbol("="); TypeOrConstructorName("Left"); VariableName("a"); Symbol("|");
    TypeOrConstructorName("Right"); VariableName("b");
    Keyword("type"); TypeOrConstructorName("Endomap"); VariableName("s"); Symbol("=");
    TypeOrConstructorName("Map"); VariableName("s"); VariableName("s"); EOF
  ] (lex "data Tree a = Leaf | Node a (Tree a) (Tree a)\ndata Either a b = Left a | Right b\ntype Endomap s = Map s s"));

  (* 15_operator_precedence_mix.cpe *)
  "15_operator_precedence_mix" >:: (fun _ -> assert_equal [
    VariableName("a"); Symbol("+"); VariableName("b"); Symbol("*"); VariableName("c");
    Symbol("<="); VariableName("d"); Symbol("//"); VariableName("e"); Symbol("&&");
    VariableName("f"); Symbol("!="); VariableName("g"); Symbol("||"); VariableName("h");
    Symbol("::"); VariableName("i"); Symbol("++"); VariableName("j"); EOF
  ] (lex "a+b*c<=d//e&&f!=g||h::i++j"));

  (* 16_err_invalid_char_escape.cpe *)
  "16_err_invalid_char_escape" >:: (fun _ ->
    match lex "'\\k'" with
    | [Error ("'\\k'", _)] -> ()
    | _ -> assert_failure "expected Error token for invalid char escape"
  );

  (* 17_err_unclosed_string.cpe *)
  "17_err_unclosed_string" >:: (fun _ ->
    match lex "\"this string is never closed" with
    | [Error ("unclosed string", _)] -> ()
    | _ -> assert_failure "expected Error token for unclosed string"
  );

  (* 18_err_nested_comment.cpe *)
  "18_err_nested_comment" >:: (fun _ ->
    match lex "(* outer (* inner *) comment *)" with
    | [Error ("(*", _)] -> ()
    | _ -> assert_failure "expected Error token for nested comment"
  );

  (* 19_err_illegal_character.cpe *)
  "19_err_illegal_character" >:: (fun _ ->
    match lex "let x = 10 @ 20" with
    | [Keyword "let"; VariableName "x"; Symbol "="; IntLit 10; Error ("@", _)] -> ()
    | _ -> assert_failure "expected Error token for illegal character @"
  );

  (* 20_err_empty_char.cpe *)
  "20_err_empty_char" >:: (fun _ ->
    match lex "''" with
    | [Error ("''", _)] -> ()
    | _ -> assert_failure "expected Error token for empty char literal"
  );

  (* 21_err_multi_char_in_char_lit.cpe *)
  "21_err_multi_char_in_char_lit" >:: (fun _ ->
    match lex "'abc'" with
    | [Error ("'abc'", _)] -> ()
    | _ -> assert_failure "expected Error token for multi-char in char lit"
  );

  (* 22_err_unclosed_char.cpe *)
  "22_err_unclosed_char" >:: (fun _ ->
    match lex "'a" with
    | [Error ("'a", _)] -> ()
    | _ -> assert_failure "expected Error token for unclosed char literal"
  )
]

let _ = run_test_tt_main tests
