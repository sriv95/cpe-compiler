open OUnit2
open CpecmuCompiler.A2_parsing

let tests = "test suite for parsing" >::: [
  (* Original sample tests *)
  "" >:: (fun _ -> assert_equal "(() ())" (pretty_parse ""));
  "simple_def" >:: (fun _ ->
    assert_equal
      "(() ((def x Integer 42)))"
      (pretty_parse "def x : Integer = 42")
  );
  "listing_1_hello_world" >:: (fun _ ->
    assert_equal
      "(() ((def main ((arg ([] String))) String \"Hello, world!\")))"
      (pretty_parse "def main (arg : [String]) : String = # ignores input\n  \"Hello, world!\"")
  );

  (* 01_hello.cpe *)
  "01_hello" >:: (fun _ ->
    assert_equal
      "(() ((def main ((arg String)) String \"Hello, world!\")))"
      (pretty_parse "def main (arg : String) : String = \"Hello, world!\"")
  );

  (* 02_arith_prec.cpe *)
  "02_arith_prec" >:: (fun _ ->
    assert_equal
      "(()\n\
      \ ((def f ((x Integer) (y Integer)) Integer (- (* (+ 2 x) y) (% (/ 3 x) 2)))\n\
      \  (def main ((arg String)) String arg)\n\
      \ )\n\
      )"
      (pretty_parse
        "def f (x : Integer) (y : Integer) : Integer = (2 + x) * y - 3 / x % 2\n\
         def main (arg : String) : String = arg")
  );

  (* 03_import.cpe *)
  "03_import" >:: (fun _ ->
    assert_equal
      "(((import pervasives) (import utils)) ((def main ((arg String)) String arg)))"
      (pretty_parse
        "import pervasives\n\
         import utils\n\n\
         def main (arg : String) : String = arg")
  );

  (* 04_data_decl.cpe *)
  "04_data_decl" >:: (fun _ ->
    assert_equal
      "(()\n\
      \ ((data (Tree a) (Leaf (Node a (Tree a) (Tree a))))\n\
      \  (data Color (Red Green Blue))\n\
      \  (def main ((arg String)) String arg)\n\
      \ )\n\
      )"
      (pretty_parse
        "data Tree a = Leaf | Node a (Tree a) (Tree a)\n\
         data Color = Red | Green | Blue\n\n\
         def main (arg : String) : String = arg")
  );

  (* 05_type_alias.cpe *)
  "05_type_alias" >:: (fun _ ->
    assert_equal
      "(()\n\
      \ ((type IntList ([] Integer))\n\
      \  (type (Pair a) (, a a))\n\
      \  (def main ((arg String)) String arg)\n\
      \ )\n\
      )"
      (pretty_parse
        "type IntList = [Integer]\n\
         type Pair a = (a, a)\n\n\
         def main (arg : String) : String = arg")
  );

  (* 06_if_nested.cpe *)
  "06_if_nested" >:: (fun _ ->
    assert_equal
      "(()\n\
      \ ((def sign ((x Integer)) Integer (if (< x 0) (- 0 1) (if (== x 0) 0 1)))\n\
      \  (def main ((arg String)) String arg)\n\
      \ )\n\
      )"
      (pretty_parse
        "def sign (x : Integer) : Integer =\n\
        \  if x < 0 then 0 - 1\n\
        \  else if x == 0 then 0\n\
        \  else 1\n\n\
         def main (arg : String) : String = arg")
  );

  (* 07_let_simple.cpe *)
  "07_let_simple" >:: (fun _ ->
    assert_equal
      "(()\n\
      \ ((def f ((x Integer)) Integer (let ((a (+ x 1))) (* a a)))\n\
      \  (def main ((arg String)) String arg)\n\
      \ )\n\
      )"
      (pretty_parse
        "def f (x : Integer) : Integer =\n\
        \  let a = x + 1\n\
        \  in a * a\n\n\
         def main (arg : String) : String = arg")
  );

  (* 08_let_function.cpe *)
  "08_let_function" >:: (fun _ ->
    assert_equal
      "(()\n\
      \ ((def\n\
      \   f\n\
      \   ((x Integer))\n\
      \   ([] Integer)\n\
      \   (let\n\
      \    ((go ((n Integer)) ([] Integer) (if (== n 0) [] (:: n (go (- n 1))))))\n\
      \    (go x)\n\
      \   )\n\
      \  )\n\
      \  (def main ((arg String)) String arg)\n\
      \ )\n\
      )"
      (pretty_parse
        "def f (x : Integer) : [Integer] =\n\
        \  let go (n : Integer) : [Integer] =\n\
        \        if n == 0 then [] else n :: go (n - 1)\n\
        \  in go x\n\n\
         def main (arg : String) : String = arg")
  );

  (* 09_match_basic.cpe *)
  "09_match_basic" >:: (fun _ ->
    assert_equal
      "(()\n\
      \ ((def\n\
      \   length\n\
      \   ((xs ([] a)))\n\
      \   Integer\n\
      \   (match xs ((-> [] 0) (-> (:: hd tl) (+ 1 (length tl)))))\n\
      \  )\n\
      \  (def main ((arg String)) String arg)\n\
      \ )\n\
      )"
      (pretty_parse
        "def length (xs : [a]) : Integer =\n\
        \  match xs with\n\
        \  | [] -> 0\n\
        \  | hd::tl -> 1 + length tl\n\n\
         def main (arg : String) : String = arg")
  );

  (* 10_match_nested_paren.cpe *)
  "10_match_nested_paren" >:: (fun _ ->
    assert_equal
      "(()\n\
      \ ((def\n\
      \   f\n\
      \   ((xs ([] Integer)) (ys ([] Integer)))\n\
      \   Integer\n\
      \   (match\n\
      \    xs\n\
      \    ((-> [] 0)\n\
      \     (-> (:: x rest) (match ys ((-> [] x) (-> (:: y _) (+ x y)))))\n\
      \     (-> _ 1)\n\
      \    )\n\
      \   )\n\
      \  )\n\
      \  (def main ((arg String)) String arg)\n\
      \ )\n\
      )"
      (pretty_parse
        "def f (xs : [Integer]) (ys : [Integer]) : Integer =\n\
        \  match xs with\n\
        \  | [] -> 0\n\
        \  | x::rest -> (match ys with\n\
        \                | [] -> x\n\
        \                | y::_ -> x + y)\n\
        \  | _ -> 1\n\n\
         def main (arg : String) : String = arg")
  );

  (* 11_match_nested_last.cpe *)
  "11_match_nested_last" >:: (fun _ ->
    assert_equal
      "(()\n\
      \ ((def\n\
      \   f\n\
      \   ((xs ([] Integer)) (ys ([] Integer)))\n\
      \   Integer\n\
      \   (match\n\
      \    xs\n\
      \    ((-> [] 0) (-> (:: x rest) (match ys ((-> [] x) (-> (:: y _) (+ x y))))))\n\
      \   )\n\
      \  )\n\
      \  (def main ((arg String)) String arg)\n\
      \ )\n\
      )"
      (pretty_parse
        "def f (xs : [Integer]) (ys : [Integer]) : Integer =\n\
        \  match xs with\n\
        \  | [] -> 0\n\
        \  | x::rest ->\n\
        \      match ys with\n\
        \      | [] -> x\n\
        \      | y::_ -> x + y\n\n\
         def main (arg : String) : String = arg")
  );

  (* 12_lambda.cpe *)
  "12_lambda" >:: (fun _ ->
    assert_equal
      "(()\n\
      \ ((def\n\
      \   addAll\n\
      \   ((xs ([] Integer)))\n\
      \   Integer\n\
      \   (((foldl (\\ ((acc Integer) (x Integer)) Integer (+ acc x))) 0) xs)\n\
      \  )\n\
      \  (def main ((arg String)) String arg)\n\
      \ )\n\
      )"
      (pretty_parse
        "def addAll (xs : [Integer]) : Integer =\n\
        \  foldl (\\ (acc : Integer) (x : Integer) : Integer => acc + x) 0 xs\n\n\
         def main (arg : String) : String = arg")
  );

  (* 13_tuple_pattern.cpe *)
  "13_tuple_pattern" >:: (fun _ ->
    assert_equal
      "(()\n\
      \ ((def swap ((p (, a b))) (, b a) (match p ((-> (, x y) (, y x)))))\n\
      \  (def main ((arg String)) String arg)\n\
      \ )\n\
      )"
      (pretty_parse
        "def swap (p : (a, b)) : (b, a) =\n\
        \  match p with\n\
        \  | (x, y) -> (y, x)\n\n\
         def main (arg : String) : String = arg")
  );

  (* 14_compose_apply.cpe *)
  "14_compose_apply" >:: (fun _ ->
    assert_equal
      "(()\n\
      \ ((def f ((xs ([] Integer))) String ((. showIntList (. sort (map inc))) xs))\n\
      \  (def g ((x Integer)) Integer ((((h a) b) (c x)) d))\n\
      \  (def main ((arg String)) String arg)\n\
      \ )\n\
      )"
      (pretty_parse
        "def f (xs : [Integer]) : String =\n\
        \  showIntList . sort . map inc $ xs\n\n\
         def g (x : Integer) : Integer = h a b (c x) d\n\n\
         def main (arg : String) : String = arg")
  );

  (* 15_literals.cpe *)
  "15_literals" >:: (fun _ ->
    assert_equal
      "(()\n\
      \ ((def a ((u ())) Char 'x')\n\
      \  (def b ((u ())) Char '\\\\n')\n\
      \  (def c ((u ())) String \"tab\\\\there \\\" quoted\\ \"\")\n\
      \  (def d ((u ())) Double 3.14)\n\
      \  (def e ((u ())) () ())\n\
      \  (def main ((arg String)) String arg)\n\
      \ )\n\
      )"
      (pretty_parse
        "def a (u : ()) : Char = 'x'\n\
         def b (u : ()) : Char = '\\n'\n\
         def c (u : ()) : String = \"tab\\there \\\"quoted\\\"\"\n\
         def d (u : ()) : Double = 3.14\n\
         def e (u : ()) : () = ()\n\n\
         def main (arg : String) : String = arg")
  );

  (* 16_logic_compare_unary.cpe *)
  "16_logic_compare_unary" >:: (fun _ ->
    assert_equal
      "(()\n\
      \ ((def\n\
      \   f\n\
      \   ((a Integer) (c Char))\n\
      \   Bool\n\
      \   (|| (&& (>= a 0) (<= a 9)) (&& (== c 'x') (!= (- a) 5)))\n\
      \  )\n\
      \  (def main ((arg String)) String arg)\n\
      \ )\n\
      )"
      (pretty_parse
        "def f (a : Integer) (c : Char) : Bool =\n\
        \  a >= 0 && a <= 9 || c == 'x' && -a != 5\n\n\
         def main (arg : String) : String = arg  # comment ท้ายบรรทัด")
  );

  (* 17_err_missing_equal.cpe *)
  "17_err_missing_equal" >:: (fun _ ->
    match parse "def f (x : Integer) : Integer\n  x + 1" with
    | None -> ()
    | Some _ -> assert_failure "expected syntax error for missing equal sign"
  );

  (* 18_err_unclosed_paren.cpe *)
  "18_err_unclosed_paren" >:: (fun _ ->
    match parse "def f (x : Integer) : Integer = (x + 1 * 2" with
    | None -> ()
    | Some _ -> assert_failure "expected syntax error for unclosed parenthesis"
  );

  (* 19_err_if_no_else.cpe *)
  "19_err_if_no_else" >:: (fun _ ->
    match parse "def f (x : Integer) : Integer = if x < 0 then 1" with
    | None -> ()
    | Some _ -> assert_failure "expected syntax error for if without else"
  );

  (* 20_err_match_no_arrow.cpe *)
  "20_err_match_no_arrow" >:: (fun _ ->
    match parse "def f (xs : [Integer]) : Integer =\n  match xs with\n  | [] 0\n  | hd::tl -> hd" with
    | None -> ()
    | Some _ -> assert_failure "expected syntax error for match case without arrow"
  );
]

let _ = run_test_tt_main tests
