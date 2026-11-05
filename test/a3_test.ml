open OUnit2
open CpecmuCompiler
open CpecmuCompiler.Typechecker

(* every program gets a valid main so that only the code under test matters *)
let main_def = "\ndef main (arg : [String]) : String = \"\"\n"

let check ?(modules = []) src =
  let load name =
    Option.bind (List.assoc_opt name modules) A2_parsing.parse in
  match A2_parsing.parse src with
  | None -> assert_failure ("syntax error in test program:\n" ^ src)
  | Some p ->
      (match typecheck_result ~load p with
       | Ok _ -> None
       | Error msg -> Some msg)

let accepts ?modules ?(main = true) src =
  fun _ ->
    match check ?modules (if main then src ^ main_def else src) with
    | None -> ()
    | Some msg -> assert_failure ("expected to typecheck but got: " ^ msg)

let rejects ?modules ?(main = true) ?contains src =
  fun _ ->
    match check ?modules (if main then src ^ main_def else src) with
    | None -> assert_failure "expected a type error but program typechecked"
    | Some msg ->
        (match contains with
         | None -> ()
         | Some sub ->
             let found =
               let n = String.length sub and m = String.length msg in
               let rec go i = i + n <= m && (String.sub msg i n = sub || go (i + 1)) in
               go 0
             in
             if not found then
               assert_failure (Printf.sprintf "error %S does not mention %S" msg sub))

let tests = "typechecking" >::: [
  "largest integer literal" >:: (fun _ ->
    assert_bool "2^62-1 is accepted"
      (A2_parsing.parse "def x : Integer = 4611686018427387903" <> None));
  "integer literal out of range" >:: (fun _ ->
    assert_bool "2^62 is rejected"
      (A2_parsing.parse "def x : Integer = 4611686018427387904" = None));

  "literals" >:: accepts
    "def f (u : ()) : (Integer, (Double, (Bool, (Char, String)))) =\n\
    \  (1, (2.5, (True, ('c', \"s\"))))";

  "arith int" >:: accepts "def f (x : Integer) : Integer = (x + 1) * 2 - x // 3 % 4";
  "arith double" >:: accepts "def f (x : Double) : Double = x * 2.5 - 1.0 / x";
  "int / int is Double" >:: accepts "def f (x : Integer) : Double = x / 2";
  "int / int is not Integer" >:: rejects "def f (x : Integer) : Integer = x / 2";
  "coerce left" >:: accepts "def f (x : Integer) (y : Double) : Double = x + y";
  "coerce right" >:: accepts "def f (x : Integer) (y : Double) : Double = y * x";
  "double // integer" >:: accepts "def f (x : Double) : Integer = x // 2";
  "double // double" >:: rejects "def f (x : Double) : Integer = x // 2.0";
  "negate" >:: accepts "def f (x : Double) (y : Integer) : (Double, Integer) = (-x, -y)";
  "negate bool" >:: rejects "def f (x : Bool) : Bool = -x";
  "arith on bool" >:: rejects ~contains:"operator +" "def f (x : Integer) : Integer = x + True";

  "compare numeric" >:: accepts "def f (x : Integer) (y : Integer) : Bool = x < y && x >= 0 || x != y";
  "compare mixed numeric" >:: rejects "def f (x : Integer) (y : Double) : Bool = x < y";
  "less-than on chars" >:: rejects "def f (x : Char) (y : Char) : Bool = x < y";
  "equality on chars" >:: accepts "def f (x : Char) (y : Char) : Bool = x == y";
  "equality on lists" >:: accepts "def f (x : [Integer]) : Bool = x == [] || x != 1::[]";
  "equality mismatch" >:: rejects "def f (x : Integer) : Bool = x == 'c'";
  "bool ops need bool" >:: rejects "def f (x : Integer) : Bool = x && True";

  "lists" >:: accepts
    "def f (x : Integer) : [Integer] = x :: x :: [] ++ (1 :: [])";
  "cons wrong element" >:: rejects "def f (x : Integer) : [Integer] = 'a' :: []";
  "append different lists" >:: rejects "def f (x : [Integer]) (y : [Char]) : [Integer] = x ++ y";
  "string is list of char" >:: accepts "def f (s : String) : [Char] = s ++ \"x\"";

  "if" >:: accepts "def f (x : Integer) : Integer = if x < 0 then 0 - x else x";
  "if non-bool condition" >:: rejects "def f (x : Integer) : Integer = if x then 1 else 2";
  "if branch mismatch" >:: rejects "def f (x : Integer) : Integer = if x < 0 then 1 else False";

  "return type mismatch" >:: rejects ~contains:"expected Bool" "def f (x : Integer) : Bool = x + 1";
  "unbound variable" >:: rejects ~contains:"unbound variable y" "def f (x : Integer) : Integer = x + y";
  "duplicate parameter" >:: rejects ~contains:"duplicate parameter"
    "def f (x : Integer) (x : Integer) : Integer = x";
  "duplicate definition" >:: rejects ~contains:"duplicate definition"
    "def f (x : Integer) : Integer = x\ndef f (y : Integer) : Integer = y";
  "redefine builtin" >:: rejects "def ord (c : Char) : Integer = 0";

  (* application and functions *)
  "application" >:: accepts
    "def inc (x : Integer) : Integer = x + 1\n\
     def f (x : Integer) : Integer = inc (inc x)";
  "apply wrong arg" >:: rejects
    "def inc (x : Integer) : Integer = x + 1\ndef f (b : Bool) : Integer = inc b";
  "apply non-function" >:: rejects "def f (x : Integer) : Integer = x 1";
  "partial application" >:: accepts
    "def add (x : Integer) (y : Integer) : Integer = x + y\n\
     def f (u : ()) : Integer -> Integer = add 1";
  "too many args" >:: rejects
    "def add (x : Integer) (y : Integer) : Integer = x + y\n\
     def f (u : ()) : Integer = add 1 2 3";
  "unit param" >:: accepts
    "def one () : Integer = 1\ndef f (u : ()) : Integer = one ()";
  "composition and $" >:: accepts
    "def inc (x : Integer) : Integer = x + 1\n\
     def f (x : Integer) : Integer = inc . inc $ x";
  "composition mismatch" >:: rejects
    "def inc (x : Integer) : Integer = x + 1\n\
     def f (x : Bool) : Integer = inc . inc $ x";
  "mutual recursion" >:: accepts
    "def isEven (x : Integer) : Bool =\n\
    \  if x == 0 then True else if x < 0 then isEven (-x) else isOdd (x-1)\n\
     def isOdd (x : Integer) : Bool =\n\
    \  if x == 0 then False else if x < 0 then isOdd (-x) else isEven (x-1)";
  "ord chr error" >:: accepts
    "def f (c : Char) : Char = chr (ord c + 1)\n\
     def g (x : Integer) : Integer = if x > 0 then x else error \"neg\"";
  "error has any type" >:: accepts
    "def f (x : Integer) : (Bool, [Char]) = error \"boom\"";

  (* polymorphism *)
  "polymorphic function used at two types" >:: accepts
    "def id (x : a) : a = x\n\
     def f (u : ()) : (Integer, Bool) = (id 1, id True)";
  "map and friends" >:: accepts
    "def map (f : a -> b) (xs : [a]) : [b] =\n\
    \  match xs with\n\
    \  | [] -> []\n\
    \  | hd::tl -> f hd :: map f tl\n\
     def foldr (accum : a -> b -> b) (init : b) (xs : [a]) : b =\n\
    \  match xs with\n\
    \  | [] -> init\n\
    \  | hd::tl -> accum hd (foldr accum init tl)\n\
     def f (xs : [Integer]) : [Bool] = map (\\ (x : Integer) : Bool => x > 0) xs\n\
     def g (xs : [Integer]) : Integer = foldr (\\ (x : Integer) (acc : Integer) : Integer => x + acc) 0 xs";
  "polymorphic misuse" >:: rejects
    "def map (f : a -> b) (xs : [a]) : [b] =\n\
    \  match xs with\n\
    \  | [] -> []\n\
    \  | hd::tl -> f hd :: map f tl\n\
     def f (xs : [Integer]) : [Bool] = map (\\ (x : Bool) : Bool => x) xs";
  "rigid type variable" >:: rejects
    "def f (x : a) : Integer = x";
  "rigid type variables differ" >:: rejects
    "def f (x : a) (y : b) : a = y";
  "swap" >:: accepts
    "def swap (p : (a, b)) : (b, a) =\n\
    \  match p with\n\
    \  | (x, y) -> (y, x)";

  (* let *)
  "let var" >:: accepts "def f (x : Integer) : Integer = let a = x + 1 in a * a";
  "let and" >:: accepts
    "def f (x : Integer) : Integer = let a = x + 1 and b = x - 1 in a * b";
  "let var cannot see earlier var" >:: rejects ~contains:"unbound variable a"
    "def f (x : Integer) : Integer = let a = x + 1 and b = a in b";
  "let duplicate" >:: rejects ~contains:"duplicate binding"
    "def f (x : Integer) : Integer = let a = 1 and a = 2 in a";
  "let function" >:: accepts
    "def f (x : Integer) : Integer =\n\
    \  let double (n : Integer) : Integer = n * 2 in double x";
  "let recursive function" >:: accepts
    "def f (x : Integer) : Integer =\n\
    \  let fact (n : Integer) : Integer = if n == 0 then 1 else n * fact (n - 1)\n\
    \  in fact x";
  "let mutual recursion" >:: accepts
    "def hofstadter (n : Integer) : (Integer, Integer) =\n\
    \  let f (n : Integer) : Integer =\n\
    \        if n == 0 then 1 else n - m (f (n-1))\n\
    \  and m (n : Integer) : Integer =\n\
    \        if n == 0 then 0 else n - f (m (n-1))\n\
    \  in (f n, m n)";
  "let function not visible outside" >:: rejects
    "def f (x : Integer) : Integer = (let g (n : Integer) : Integer = n in g x) + g x";
  "let polymorphic function" >:: accepts
    "def f (u : ()) : (Integer, Bool) =\n\
    \  let id (x : b) : b = x in (id 1, id True)";
  "let function uses enclosing type variable" >:: accepts
    "def f (x : a) : a =\n\
    \  let g (y : a) : a = y in g x";
  "nonterminating let function" >:: accepts
    "def bot () : Integer =\n\
    \  let forever (x : Integer) : Integer = forever x\n\
    \  in forever 42";
  "let function body mismatch" >:: rejects
    "def f (x : Integer) : Integer =\n\
    \  let g (n : Integer) : Bool = n + 1 in 0";

  (* lambdas *)
  "lambda" >:: accepts
    "def apply (f : Integer -> Integer) (x : Integer) : Integer = f x\n\
     def g (x : Integer) : Integer = apply (\\ (n : Integer) : Integer => n + x) 10";
  "multi-parameter lambda" >:: accepts
    "def f (u : ()) : Integer -> Integer -> Integer =\n\
    \  \\ (a : Integer) (b : Integer) : Integer => a + b";
  "lambda body mismatch" >:: rejects
    "def f (u : ()) : Integer -> Bool = \\ (a : Integer) : Bool => a + 1";
  "lambda duplicate parameters" >:: rejects ~contains:"duplicate parameter"
    "def f (u : ()) : Integer -> Integer -> Integer =\n\
    \  \\ (a : Integer) (a : Integer) : Integer => a";
  "lambda is not recursive" >:: rejects
    "def f (u : ()) : Integer -> Integer = \\ (n : Integer) : Integer => f n";

  (* pattern matching *)
  "match list" >:: accepts
    "def length (xs : [a]) : Integer =\n\
    \  match xs with\n\
    \  | [] -> 0\n\
    \  | hd::tl -> 1 + length tl";
  "match nested" >:: accepts
    "def f (l : [Integer]) : Bool =\n\
    \  match l with\n\
    \  | [] -> True\n\
    \  | x1::x2::xs -> x1 == x2\n\
    \  | hd::tl -> False";
  "match wildcard and literals" >:: accepts
    "def f (n : Integer) (c : Char) (s : String) (b : Bool) : Integer =\n\
    \  match (n, (c, (s, b))) with\n\
    \  | (0, ('a', (\"x\", True))) -> 1\n\
    \  | (_, _) -> 2";
  "match unit" >:: accepts
    "def f (u : ()) : Integer = match u with | () -> 1";
  "match branch mismatch" >:: rejects
    "def f (xs : [Integer]) : Integer =\n\
    \  match xs with\n\
    \  | [] -> 0\n\
    \  | hd::tl -> True";
  "pattern type mismatch" >:: rejects
    "def f (xs : [Integer]) : Integer =\n\
    \  match xs with\n\
    \  | (a, b) -> a";
  "literal pattern mismatch" >:: rejects
    "def f (n : Integer) : Integer = match n with | 'c' -> 1 | _ -> 2";
  "pattern binds element type" >:: rejects
    "def f (xs : [Integer]) : Bool =\n\
    \  match xs with\n\
    \  | hd::tl -> hd\n\
    \  | [] -> True";
  "pattern variable bound twice" >:: rejects ~contains:"bound more than once"
    "def f (p : (Integer, Integer)) : Integer = match p with | (a, a) -> a";
  "pattern variable scope" >:: rejects ~contains:"unbound variable hd"
    "def f (xs : [Integer]) : Integer =\n\
    \  (match xs with | hd::tl -> hd | [] -> 0) + hd";

  (* data types *)
  "data and match" >:: accepts
    "data Tree a = Leaf | Node a (Tree a) (Tree a)\n\
     def size (t : Tree a) : Integer =\n\
    \  match t with\n\
    \  | Leaf -> 0\n\
    \  | Node v l r -> 1 + size l + size r\n\
     def preorder (t : Tree a) : [a] =\n\
    \  match t with\n\
    \  | Leaf -> []\n\
    \  | Node v l r -> v :: preorder l ++ preorder r\n\
     def build (u : ()) : Tree Integer = Node 1 Leaf (Node 2 Leaf Leaf)";
  "constructor wrong argument" >:: rejects
    "data Tree a = Leaf | Node a (Tree a) (Tree a)\n\
     def f (u : ()) : Tree Integer = Node True Leaf Leaf";
  "constructor result type" >:: rejects
    "data Tree a = Leaf | Node a (Tree a) (Tree a)\n\
     def f (u : ()) : Tree Bool = Node 1 Leaf Leaf";
  "constructor wrong arity in pattern" >:: rejects ~contains:"expects 3"
    "data Tree a = Leaf | Node a (Tree a) (Tree a)\n\
     def f (t : Tree Integer) : Integer = match t with | Node v l -> v | Leaf -> 0";
  "unknown constructor" >:: rejects ~contains:"unknown constructor"
    "def f (u : ()) : Integer = match 1 with | Nope -> 1 | _ -> 2";
  "pattern of other data type" >:: rejects
    "data A = A1 | A2\ndata B = B1\n\
     def f (a : A) : Integer = match a with | B1 -> 1 | _ -> 2";
  "constructor as a function" >:: accepts
    "data Box a = Box a\n\
     def mk (u : ()) : Integer -> Box Integer = Box";
  "type and constructor namespaces are separate" >:: accepts
    "data Box a = Box a\n\
     def unbox (b : Box a) : a = match b with | Box x -> x";
  "mutually recursive data" >:: accepts
    "data Even = Zero | ESucc Odd\ndata Odd = OSucc Even\n\
     def f (u : ()) : Even = ESucc (OSucc Zero)";
  "multi-parameter data" >:: accepts
    "data Either a b = Left a | Right b\n\
     def f (e : Either Integer Bool) : Bool =\n\
    \  match e with | Left n -> n > 0 | Right b -> b";
  "data using lists and pairs and functions" >:: accepts
    "data Rose a = Rose a [Rose a]\n\
     data Fn a b = Fn (a -> b) (a, b)\n\
     def f (r : Rose Integer) : Integer = match r with | Rose v _ -> v";
  "duplicate type" >:: rejects ~contains:"duplicate type name"
    "data T = A\ndata T = B";
  "duplicate type vs synonym" >:: rejects ~contains:"duplicate type name"
    "data T = A\ntype T = Integer";
  "duplicate constructor" >:: rejects ~contains:"duplicate constructor"
    "data T = A | B\ndata U = A";
  "duplicate constructor within a type" >:: rejects ~contains:"duplicate constructor"
    "data T = A | A";
  "builtin type redefinition" >:: rejects ~contains:"duplicate type name" "data Bool = Yes | No";
  "undeclared type variable" >:: rejects ~contains:"not declared"
    "data T a = C b";
  "duplicate type parameter" >:: rejects ~contains:"duplicate type parameter"
    "data T a a = C a";
  "unknown type" >:: rejects ~contains:"unknown type"
    "def f (x : Foo) : Integer = 1";
  "wrong number of type arguments" >:: rejects ~contains:"expects 1"
    "data Tree a = Leaf\ndef f (x : Tree) : Integer = 1";
  "too many type arguments" >:: rejects ~contains:"expects 1"
    "data Tree a = Leaf\ndef f (x : Tree Integer Bool) : Integer = 1";
  "type variable cannot be applied" >:: rejects
    "def f (x : m Integer) : Integer = 1";

  (* type synonyms *)
  "type synonym" >:: accepts
    "type Name = String\ntype Pair a = (a, a)\ntype IntPair = Pair Integer\n\
     def f (n : Name) (p : IntPair) : [Char] = n ++ \"!\"\n\
     def g (p : Pair Integer) : IntPair = p";
  "synonym declared after use" >:: accepts
    "def f (x : Later) : Integer = x\ntype Later = Integer";
  "synonym of data" >:: accepts
    "data Map k v = Empty | Entry k v (Map k v)\n\
     type Endomap s = Map s s\n\
     def f (m : Endomap Integer) : Map Integer Integer = m";
  "synonym with free variable" >:: rejects ~contains:"not a parameter"
    "type Bad a = (a, b)";
  "cyclic synonym" >:: rejects ~contains:"cyclic"
    "type A = B\ntype B = A";
  "self-referential synonym" >:: rejects ~contains:"cyclic"
    "type A = [A]";
  "synonym arity" >:: rejects ~contains:"expects 1"
    "type Pair a = (a, a)\ndef f (p : Pair) : Integer = 1";

  (* top-level variables *)
  "top-level variable" >:: accepts
    "def x : Integer = 1\ndef y : Integer = x + 1\ndef f (n : Integer) : Integer = n + y";
  "forward reference" >:: rejects ~contains:"defined later"
    "def x : Integer = y + 1\ndef y : Integer = 2";
  "circular variables" >:: rejects
    "def x : Integer = y + 1\ndef y : Integer = x - 1";
  "self-referencing variable" >:: rejects ~contains:"refers to itself"
    "def x : Integer = x + 1";
  "variable using later function" >:: accepts
    "def x : Integer = f 1\ndef f (n : Integer) : Integer = n + 1";
  "variable reaching later variable through a function" >:: rejects
    "def x : Integer = f 1\ndef f (n : Integer) : Integer = n + y\ndef y : Integer = 2";
  "function using later variable" >:: accepts
    "def f (n : Integer) : Integer = n + y\ndef y : Integer = 2";
  "local binding shadows later variable" >:: accepts
    "def x : Integer = let y = 1 in y\ndef y : Integer = 2";
  "parameter shadows later variable" >:: accepts
    "def x : Integer = f 1\ndef f (y : Integer) : Integer = y\ndef y : Integer = 2";

  (* main *)
  "main has right type" >:: accepts ~main:false
    "def main (arg : [String]) : String = \"hi\"";
  "main absent" >:: accepts ~main:false "def f (x : Integer) : Integer = x";
  "main takes a string" >:: rejects ~main:false ~contains:"main must have type"
    "def main (arg : String) : String = arg";
  "main returns int" >:: rejects ~main:false ~contains:"main must have type"
    "def main (arg : [String]) : Integer = 1";
  "main is a variable" >:: rejects ~main:false ~contains:"main must have type"
    "def main : String = \"x\"";

  (* imports *)
  "import" >:: accepts
    ~modules:["lib", "def inc (x : Integer) : Integer = x + 1"]
    "import lib\ndef f (x : Integer) : Integer = inc x";
  "import with namespace" >:: accepts
    ~modules:["lib", "def inc (x : Integer) : Integer = x + 1"]
    "import lib as l\ndef f (x : Integer) : Integer = l!inc x";
  "namespaced name needs prefix" >:: rejects ~contains:"unbound variable inc"
    ~modules:["lib", "def inc (x : Integer) : Integer = x + 1"]
    "import lib as l\ndef f (x : Integer) : Integer = inc x";
  "unknown namespace" >:: rejects ~contains:"unknown namespace"
    "def f (x : Integer) : Integer = l!inc x";
  "unknown name in namespace" >:: rejects ~contains:"not defined in namespace"
    ~modules:["lib", "def inc (x : Integer) : Integer = x + 1"]
    "import lib as l\ndef f (x : Integer) : Integer = l!dec x";
  "imported function types are checked" >:: rejects
    ~modules:["lib", "def inc (x : Integer) : Integer = x + 1"]
    "import lib as l\ndef f (x : Bool) : Integer = l!inc x";
  "import clash" >:: rejects ~contains:"duplicate"
    ~modules:["a", "def inc (x : Integer) : Integer = x + 1";
              "b", "def inc (x : Integer) : Integer = x + 2"]
    "import a\nimport b";
  "import clash with own definition" >:: rejects ~contains:"duplicate definition"
    ~modules:["lib", "def inc (x : Integer) : Integer = x + 1"]
    "import lib\ndef inc (x : Integer) : Integer = x";
  "same name through namespaces" >:: accepts
    ~modules:["a", "def inc (x : Integer) : Integer = x + 1";
              "b", "def inc (x : Integer) : Integer = x + 2"]
    "import a as a\nimport b as b\n\
     def f (x : Integer) : Integer = a!inc x + b!inc x";
  "imported main in a namespace" >:: accepts
    ~modules:["lib", "def main (arg : [String]) : String = \"lib\""]
    "import lib as l";
  "imported main clashes" >:: rejects ~contains:"duplicate"
    ~modules:["lib", "def main (arg : [String]) : String = \"lib\""]
    "import lib";
  "imported data types" >:: accepts
    ~modules:["tree", "data Tree a = Leaf | Node a (Tree a) (Tree a)"]
    "import tree\n\
     def f (t : Tree Integer) : Integer = match t with | Leaf -> 0 | Node v _ _ -> v";
  "imported types clash" >:: rejects ~contains:"duplicate type name"
    ~modules:["tree", "data Tree a = Leaf | Node a (Tree a) (Tree a)"]
    "import tree\ndata Tree = T";
  "types of namespaced import do not clash" >:: accepts
    ~modules:["tree", "data Tree a = Leaf | Node a (Tree a) (Tree a)"]
    "import tree as t\ndata Tree = Leaf";
  "namespaced import keeps its own types apart" >:: rejects
    ~modules:["tree", "data Tree = Leaf\ndef leaf (u : ()) : Tree = Leaf"]
    "import tree as t\ndata Tree = Leaf\ndef f (u : ()) : Tree = t!leaf ()";
  "imports are not re-exported" >:: rejects ~contains:"unbound variable inc"
    ~modules:["lib", "import base\ndef twice (x : Integer) : Integer = inc (inc x)";
              "base", "def inc (x : Integer) : Integer = x + 1"]
    "import lib\ndef f (x : Integer) : Integer = inc x";
  "transitive imports are checked" >:: accepts
    ~modules:["lib", "import base\ndef twice (x : Integer) : Integer = inc (inc x)";
              "base", "def inc (x : Integer) : Integer = x + 1"]
    "import lib\ndef f (x : Integer) : Integer = twice x";
  "type error inside imported module" >:: rejects
    ~modules:["lib", "def bad (x : Integer) : Bool = x"]
    "import lib";
  "missing import" >:: rejects ~contains:"cannot import"
    "import nothere";
  "cyclic import" >:: rejects ~contains:"cyclic import"
    ~modules:["a", "import b"; "b", "import a"]
    "import a";
]

let _ = run_test_tt_main tests
