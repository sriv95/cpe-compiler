(* End-to-end tests: compile CPE programs through LLVM with clang, run them
   and compare what they print. *)
open OUnit2
open CpecmuCompiler

(* a small library shared by the programs below *)
let fmt_module = {|
def reverse (xs : [a]) : [a] =
  let go (acc : [a]) (l : [a]) : [a] =
        match l with
        | [] -> acc
        | h::t -> go (h::acc) t
  in go [] xs

def showInt (x : Integer) : String =
  if x == 0 then "0"
  else if x < 0 then '-' :: showInt (0 - x)
  else
    let digits (n : Integer) (acc : String) : String =
          if n == 0 then acc else digits (n // 10) (chr (n % 10 + 48) :: acc)
    in digits x ""

def showBool (b : Bool) : String = if b then "True" else "False"

def unwords (strs : [String]) : String =
  match strs with
  | [] -> ""
  | hd::[] -> hd
  | hd::tl -> hd ++ " " ++ unwords tl

def map (f : a -> b) (xs : [a]) : [b] =
  match xs with
  | [] -> []
  | hd::tl -> f hd :: map f tl

def filter (p : a -> Bool) (xs : [a]) : [a] =
  match xs with
  | [] -> []
  | hd::tl -> if p hd then hd :: filter p tl else filter p tl

def foldl (accum : b -> a -> b) (init : b) (xs : [a]) : b =
  match xs with
  | [] -> init
  | hd::tl -> foldl accum (accum init hd) tl

def foldr (accum : a -> b -> b) (init : b) (xs : [a]) : b =
  match xs with
  | [] -> init
  | hd::tl -> accum hd (foldr accum init tl)

def range (lo : Integer) (hi : Integer) : [Integer] =
  if lo > hi then [] else lo :: range (lo + 1) hi
|}

let counter = ref 0

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let write_file path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

(* compile and run; result is (exit status, stdout, stderr) *)
let run ?(modules = []) ?(args = []) src =
  incr counter;
  let dir = Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "cpe_e2e_%d_%d" (Unix.getpid ()) !counter) in
  Sys.mkdir dir 0o755;
  let modules = ("fmt", fmt_module) :: modules in
  List.iter (fun (n, s) -> write_file (Filename.concat dir (n ^ ".cpe")) s) modules;
  let main = Filename.concat dir "main.cpe" in
  write_file main src;
  let exe = Filename.concat dir "prog" in
  (match Driver.compile_file ~opt:"-O1" main exe with
   | Ok () -> ()
   | Error msg -> assert_failure ("compilation failed: " ^ msg));
  let out = Filename.concat dir "out.txt" and err = Filename.concat dir "err.txt" in
  let cmd = Printf.sprintf "%s %s > %s 2> %s" (Filename.quote exe)
      (String.concat " " (List.map Filename.quote args))
      (Filename.quote out) (Filename.quote err) in
  let status = Sys.command cmd in
  (status, read_file out, read_file err)

let prints ?modules ?args expected src _ =
  match run ?modules ?args src with
  | 0, out, _ -> assert_equal ~printer:(fun s -> s) (expected ^ "\n") out
  | status, out, err ->
      assert_failure (Printf.sprintf "exit status %d\nstdout: %s\nstderr: %s" status out err)

let fails ?modules ?args ~stderr_has src _ =
  match run ?modules ?args src with
  | 0, out, _ -> assert_failure ("expected a failure but the program printed: " ^ out)
  | status, _, err ->
      assert_equal ~printer:string_of_int 1 status;
      let contains s sub =
        let n = String.length sub and m = String.length s in
        let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in go 0 in
      if not (contains err stderr_has) then
        assert_failure (Printf.sprintf "stderr %S does not contain %S" err stderr_has)

let with_fmt body = "import fmt\n" ^ body

let tests = "end to end" >::: [
  "hello" >:: prints "Hello, world!"
    "def main (arg : [String]) : String = \"Hello, world!\"";

  "arguments" >:: prints ~args:["a"; "bc"; "def"] "a bc def"
    (with_fmt "def main (arg : [String]) : String = unwords arg");

  "integer arithmetic" >:: prints "39"
    (with_fmt "def main (arg : [String]) : String = showInt ((7 + 3) * 4 - 10 // 3 + 17 % 5)");
  "negative numbers" >:: prints "-15 -7 12"
    (with_fmt "def main (arg : [String]) : String =\n\
              \  unwords (showInt (0 - 5 * 3) :: showInt (-(3 + 4)) :: showInt (-3 * -4) :: [])");
  "big integers" >:: prints "4611686018427387903"
    (with_fmt "def main (arg : [String]) : String = showInt 4611686018427387903");

  "doubles" >:: prints "7 3 3 10"
    (with_fmt "def main (arg : [String]) : String =\n\
              \  unwords (showInt (((7 / 2) * 2.0) // 1) :: showInt ((1 + 2.5) // 1)\n\
              \           :: showInt ((7.5 % 2 * 2) // 1) :: showInt ((2.5 * 4.0) // 1) :: [])");
  "double comparison" >:: prints "True False True True"
    (with_fmt "def main (arg : [String]) : String =\n\
              \  unwords (showBool (2.5 < 3.0) :: showBool (2.5 >= 3.0)\n\
              \           :: showBool (2.5 == 2.5) :: showBool (1.5 != 2.5) :: [])");
  "negative doubles" >:: prints "-3"
    (with_fmt "def main (arg : [String]) : String = showInt ((-3.5) // 1)");

  "integer comparison" >:: prints "True False True False True True"
    (with_fmt "def main (arg : [String]) : String =\n\
              \  unwords (showBool (1 < 2) :: showBool (2 < 1) :: showBool (2 <= 2)\n\
              \           :: showBool (1 > 2) :: showBool (2 >= 2) :: showBool (1 != 2) :: [])");
  "structural equality" >:: prints "True False True False True True"
    (with_fmt
       "data T = A | B Integer (Integer, [Char])\n\
        def main (arg : [String]) : String =\n\
       \  unwords (showBool ((1::2::[]) == (1::2::[])) :: showBool ((1::2::[]) == (1::3::[]))\n\
       \           :: showBool (\"abc\" == \"abc\") :: showBool (\"abc\" == \"abd\")\n\
       \           :: showBool (B 1 (2, \"x\") == B 1 (2, \"x\")) :: showBool (A != B 1 (2, \"x\")) :: [])");
  "equality in a polymorphic function" >:: prints "True False"
    (with_fmt
       "def same (x : a) (y : a) : Bool = x == y\n\
        def main (arg : [String]) : String =\n\
       \  unwords (showBool (same \"hi\" \"hi\") :: showBool (same (1, True) (1, False)) :: [])");
  "short-circuit boolean operators" >:: prints "False True"
    (with_fmt
       "def main (arg : [String]) : String =\n\
       \  unwords (showBool (False && error \"no\") :: showBool (True || error \"no\") :: [])");

  "if and recursion" >:: prints "3628800"
    (with_fmt
       "def fact (n : Integer) : Integer = if n == 0 then 1 else n * fact (n - 1)\n\
        def main (arg : [String]) : String = showInt (fact 10)");
  "mutual recursion" >:: prints "True False"
    (with_fmt
       "def isEven (x : Integer) : Bool = if x == 0 then True else isOdd (x - 1)\n\
        def isOdd (x : Integer) : Bool = if x == 0 then False else isEven (x - 1)\n\
        def main (arg : [String]) : String =\n\
       \  unwords (showBool (isEven 100000) :: showBool (isOdd 100000) :: [])");
  "long tail recursion" >:: prints "500000500000"
    (with_fmt
       "def sum (acc : Integer) (n : Integer) : Integer =\n\
       \  if n == 0 then acc else sum (acc + n) (n - 1)\n\
        def main (arg : [String]) : String = showInt (sum 0 1000000)");
  "deep non-tail recursion" >:: prints "200000"
    (with_fmt
       "def count (n : Integer) : Integer = if n == 0 then 0 else 1 + count (n - 1)\n\
        def main (arg : [String]) : String = showInt (count 200000)");

  (* functions as values *)
  "lambda" >:: prints "11"
    (with_fmt
       "def apply (f : Integer -> Integer) (x : Integer) : Integer = f x\n\
        def main (arg : [String]) : String =\n\
       \  showInt (apply (\\ (n : Integer) : Integer => n + 1) 10)");
  "closure captures" >:: prints "7 30"
    (with_fmt
       "def adder (n : Integer) : Integer -> Integer = \\ (x : Integer) : Integer => x + n\n\
        def head (xs : [Integer]) : Integer = match xs with | h::_ -> h | [] -> error \"empty\"\n\
        def main (arg : [String]) : String =\n\
       \  let k = 10 and add3 = adder 3\n\
       \  in unwords (showInt (adder 3 4)\n\
       \              :: showInt (head (map (\\ (x : Integer) : Integer => x * k) (add3 0 :: []))) :: [])");
  "nested closures" >:: prints "123"
    (with_fmt
       "def main (arg : [String]) : String =\n\
       \  let a = 100\n\
       \  in let f (x : Integer) : Integer -> Integer =\n\
       \           \\ (y : Integer) : Integer => a + x * 10 + y\n\
       \     in showInt (f 2 3)");
  "partial application" >:: prints "6 2 3 4"
    (with_fmt
       "def add3 (a : Integer) (b : Integer) (c : Integer) : Integer = a + b + c\n\
        def main (arg : [String]) : String =\n\
       \  let add12 = add3 1 2\n\
       \  in unwords (showInt (add12 3) :: map showInt (map (add3 1 0) (1::2::3::[])))");
  "over-application" >:: prints "5"
    (with_fmt
       "def pick (b : Bool) : Integer -> Integer -> Integer =\n\
       \  if b then (\\ (x : Integer) (y : Integer) : Integer => x)\n\
       \  else (\\ (x : Integer) (y : Integer) : Integer => y)\n\
        def main (arg : [String]) : String = showInt (pick True 5 6)");
  "composition and application operators" >:: prints "12 5"
    (with_fmt
       "def inc (x : Integer) : Integer = x + 1\n\
        def double (x : Integer) : Integer = x * 2\n\
        def main (arg : [String]) : String =\n\
       \  unwords (showInt ((double . inc . inc) 4) :: (showInt $ inc 4) :: [])");
  "functions in data" >:: prints "9"
    (with_fmt
       "data Box = Box (Integer -> Integer) Integer\n\
        def run (b : Box) : Integer = match b with | Box f x -> f x\n\
        def main (arg : [String]) : String =\n\
       \  showInt (run (Box (\\ (n : Integer) : Integer => n * n) 3))");
  "constructor as a function" >:: prints "1 2 3"
    (with_fmt
       "data Box a = Box a\n\
        def unbox (b : Box a) : a = match b with | Box x -> x\n\
        def main (arg : [String]) : String =\n\
       \  unwords (map showInt (map unbox (map Box (1::2::3::[]))))");
  "fold with lambda" >:: prints "55"
    (with_fmt
       "def main (arg : [String]) : String =\n\
       \  showInt (foldl (\\ (acc : Integer) (x : Integer) : Integer => acc + x) 0 (range 1 10))");

  (* local definitions *)
  "let function" >:: prints "20"
    (with_fmt
       "def main (arg : [String]) : String =\n\
       \  let double (n : Integer) : Integer = n * 2 in showInt (double 10)");
  "local recursive function" >:: prints "120"
    (with_fmt
       "def main (arg : [String]) : String =\n\
       \  let fact (n : Integer) : Integer = if n == 0 then 1 else n * fact (n - 1)\n\
       \  in showInt (fact 5)");
  "local mutual recursion" >:: prints "6 6"
    (with_fmt
       "def main (arg : [String]) : String =\n\
       \  let f (n : Integer) : Integer =\n\
       \        if n == 0 then 1 else n - m (f (n-1))\n\
       \  and m (n : Integer) : Integer =\n\
       \        if n == 0 then 0 else n - f (m (n-1))\n\
       \  in unwords (showInt (f 10) :: showInt (m 10) :: [])");
  "local function capturing" >:: prints "13"
    (with_fmt
       "def f (a : Integer) (b : Integer) : Integer =\n\
       \  let g (x : Integer) : Integer = x + a * b in g 3\n\
        def main (arg : [String]) : String = showInt (f 2 5)");
  "local polymorphic function" >:: prints "1 True"
    (with_fmt
       "def main (arg : [String]) : String =\n\
       \  let id (x : b) : b = x\n\
       \  in unwords (showInt (id 1) :: showBool (id True) :: [])");
  "let and variables" >:: prints "5 -1"
    (with_fmt
       "def main (arg : [String]) : String =\n\
       \  let a = 2 and b = 3 in unwords (showInt (a + b) :: showInt (a - b) :: [])");
  "shadowing" >:: prints "2"
    (with_fmt
       "def main (arg : [String]) : String =\n\
       \  let x = 1 in let x = x + 1 in showInt x");

  (* data types and pattern matching *)
  "lists" >:: prints "1 2 3 4 5 5"
    (with_fmt
       "def length (xs : [a]) : Integer = match xs with | [] -> 0 | _::t -> 1 + length t\n\
        def main (arg : [String]) : String =\n\
       \  let xs = range 1 5\n\
       \  in unwords (map showInt xs ++ showInt (length xs) :: [])");
  "list functions" >:: prints "2 4 6 [] 15"
    (with_fmt
       "def main (arg : [String]) : String =\n\
       \  unwords (map showInt (map (\\ (x : Integer) : Integer => x * 2) (range 1 3))\n\
       \           ++ \"[]\" :: showInt (foldr (\\ (x : Integer) (acc : Integer) : Integer => x + acc) 0 (range 1 5)) :: [])");
  "reverse and filter" >:: prints "5 3 1"
    (with_fmt
       "def odd (x : Integer) : Bool = x % 2 == 1\n\
        def main (arg : [String]) : String =\n\
       \  unwords (map showInt (reverse (filter odd (range 1 5))))");
  "tuples" >:: prints "2 1"
    (with_fmt
       "def swap (p : (a, b)) : (b, a) = match p with | (x, y) -> (y, x)\n\
        def main (arg : [String]) : String =\n\
       \  match swap (1, 2) with | (a, b) -> unwords (showInt a :: showInt b :: [])");
  "nested patterns" >:: prints "yes no"
    (with_fmt
       "def f (l : [Integer]) : String =\n\
       \  match l with\n\
       \  | [] -> \"no\"\n\
       \  | x1::x2::xs -> if x1 == x2 then \"yes\" else \"no\"\n\
       \  | hd::tl -> \"no\"\n\
        def main (arg : [String]) : String =\n\
       \  unwords (f (3::3::[]) :: f (3::4::[]) :: [])");
  "literal patterns" >:: prints "zero one many a-b"
    (with_fmt
       "def name (n : Integer) : String =\n\
       \  match n with | 0 -> \"zero\" | 1 -> \"one\" | _ -> \"many\"\n\
        def pair (c : Char) (s : String) : String =\n\
       \  match (c, s) with | ('a', \"b\") -> \"a-b\" | _ -> \"other\"\n\
        def main (arg : [String]) : String =\n\
       \  unwords (name 0 :: name 1 :: name 7 :: pair 'a' \"b\" :: [])");
  "bool and unit patterns" >:: prints "T F u"
    (with_fmt
       "def f (b : Bool) : String = match b with | True -> \"T\" | False -> \"F\"\n\
        def g (u : ()) : String = match u with | () -> \"u\"\n\
        def main (arg : [String]) : String = unwords (f True :: f False :: g () :: [])");
  "float patterns" >:: prints "half other"
    (with_fmt
       "def f (d : Double) : String = match d with | 0.5 -> \"half\" | _ -> \"other\"\n\
        def main (arg : [String]) : String = unwords (f 0.5 :: f 1.5 :: [])");
  "trees" >:: prints "1 2 3 4 5 | 3 1 2 5 4"
    (with_fmt
       "data Tree a = Leaf | Node a (Tree a) (Tree a)\n\
        def insert (x : Integer) (t : Tree Integer) : Tree Integer =\n\
       \  match t with\n\
       \  | Leaf -> Node x Leaf Leaf\n\
       \  | Node v l r -> if x < v then Node v (insert x l) r else Node v l (insert x r)\n\
        def inorder (t : Tree a) : [a] =\n\
       \  match t with | Leaf -> [] | Node v l r -> inorder l ++ v::[] ++ inorder r\n\
        def preorder (t : Tree a) : [a] =\n\
       \  match t with | Leaf -> [] | Node v l r -> v :: preorder l ++ preorder r\n\
        def main (arg : [String]) : String =\n\
       \  let t = foldl (\\ (acc : Tree Integer) (x : Integer) : Tree Integer => insert x acc)\n\
       \                Leaf (3::1::2::5::4::[])\n\
       \  in unwords (map showInt (inorder t)) ++ \" | \" ++ unwords (map showInt (preorder t))");
  "data with mixed constructors" >:: prints "circle 3 rect 12 empty"
    (with_fmt
       "data Shape = Empty | Circle Integer | Rect Integer Integer\n\
        def describe (s : Shape) : String =\n\
       \  match s with\n\
       \  | Empty -> \"empty\"\n\
       \  | Circle r -> \"circle \" ++ showInt r\n\
       \  | Rect w h -> \"rect \" ++ showInt (w * h)\n\
        def main (arg : [String]) : String =\n\
       \  unwords (describe (Circle 3) :: describe (Rect 3 4) :: describe Empty :: [])");
  "mutually recursive data" >:: prints "2"
    (with_fmt
       "data Even = Zero | ESucc Odd\ndata Odd = OSucc Even\n\
        def evenVal (e : Even) : Integer =\n\
       \  match e with | Zero -> 0 | ESucc o -> 1 + oddVal o\n\
        def oddVal (o : Odd) : Integer = match o with | OSucc e -> 1 + evenVal e\n\
        def main (arg : [String]) : String = showInt (evenVal (ESucc (OSucc Zero)))");
  "two parameter data" >:: prints "5 true"
    (with_fmt
       "data Either a b = Left a | Right b\n\
        def f (e : Either Integer Bool) : String =\n\
       \  match e with | Left n -> showInt n | Right b -> if b then \"true\" else \"false\"\n\
        def main (arg : [String]) : String = unwords (f (Left 5) :: f (Right True) :: [])");
  "type synonyms" >:: prints "3"
    (with_fmt
       "type Name = String\ntype Pair a = (a, a)\n\
        def size (n : Name) : Integer = match n with | [] -> 0 | _::t -> 1 + size t\n\
        def sumPair (p : Pair Integer) : Integer = match p with | (a, b) -> a + b\n\
        def main (arg : [String]) : String = showInt (size \"abc\")");

  (* strings and characters *)
  "strings" >:: prints "hello world"
    (with_fmt
       "def main (arg : [String]) : String = \"hello\" ++ \" \" ++ \"world\"");
  "escape sequences" >:: prints "10 9 92 39 34"
    (with_fmt
       "def main (arg : [String]) : String =\n\
       \  unwords (showInt (ord '\\n') :: showInt (ord '\\t') :: showInt (ord '\\\\')\n\
       \           :: showInt (ord '\\'') :: showInt (ord '\\\"') :: [])");
  "ord and chr" >:: prints "98 b"
    (with_fmt
       "def main (arg : [String]) : String =\n\
       \  unwords (showInt (ord 'a' + 1) :: (chr (ord 'a' + 1) :: []) :: [])");
  "string with newline" >:: prints "a\nb"
    (with_fmt "def main (arg : [String]) : String = \"a\\nb\"");

  (* top-level variables *)
  "top-level variables" >:: prints "10 20 30"
    (with_fmt
       "def base : Integer = 10\n\
        def twice : Integer = base * 2\n\
        def total : Integer = twice + base\n\
        def main (arg : [String]) : String =\n\
       \  unwords (showInt base :: showInt twice :: showInt total :: [])");
  "top-level variable using a function" >:: prints "120"
    (with_fmt
       "def fact (n : Integer) : Integer = if n == 0 then 1 else n * fact (n - 1)\n\
        def f5 : Integer = fact 5\n\
        def main (arg : [String]) : String = showInt f5");
  "top-level function variable" >:: prints "8"
    (with_fmt
       "def double : Integer -> Integer = \\ (x : Integer) : Integer => x * 2\n\
        def main (arg : [String]) : String = showInt (double 4)");
  "unit function" >:: prints "42"
    (with_fmt
       "def answer () : Integer = 42\n\
        def main (arg : [String]) : String = showInt (answer ())");

  (* imports *)
  "import" >:: prints ~modules:["lib", "def inc (x : Integer) : Integer = x + 1"] "6"
    (with_fmt "import lib\ndef main (arg : [String]) : String = showInt (inc 5)");
  "namespaced import" >:: prints "11 12"
    ~modules:["a", "def inc (x : Integer) : Integer = x + 1";
              "b", "def inc (x : Integer) : Integer = x + 2"]
    (with_fmt
       "import a as a\nimport b as b\n\
        def main (arg : [String]) : String =\n\
       \  unwords (showInt (a!inc 10) :: showInt (b!inc 10) :: [])");
  "imported variables and data" >:: prints "seven 7"
    ~modules:["lib", "data Color = Red | Seven\n\
                      def seven : Integer = 7\n\
                      def name (c : Color) : String =\n\
                     \  match c with | Red -> \"red\" | Seven -> \"seven\"\n\
                      def pick : Color = Seven"]
    (with_fmt
       "import lib\n\
        def main (arg : [String]) : String =\n\
       \  unwords (name pick :: showInt seven :: [])");
  "import of a module that has main" >:: prints ~modules:[
      "other", "def main (arg : [String]) : String = \"other\"\n\
                def helper (x : Integer) : Integer = x * 2"] "8"
    (with_fmt
       "import other as o\n\
        def main (arg : [String]) : String = showInt (o!helper 4)");
  "transitive imports" >:: prints "3"
    ~modules:["base", "def inc (x : Integer) : Integer = x + 1";
              "mid", "import base\ndef twice (x : Integer) : Integer = inc (inc x)"]
    (with_fmt "import mid\ndef main (arg : [String]) : String = showInt (twice 1)");

  (* run-time errors *)
  "error" >:: fails ~stderr_has:"boom"
    "def main (arg : [String]) : String = error \"boom\"";
  "error output is flushed after earlier output" >:: fails ~stderr_has:"too small"
    (with_fmt
       "def f (x : Integer) : Integer = if x < 10 then error \"too small\" else x\n\
        def main (arg : [String]) : String = showInt (f 3)");
  "non-exhaustive match" >:: fails ~stderr_has:"non-exhaustive"
    (with_fmt
       "def f (xs : [Integer]) : Integer = match xs with | [] -> 0\n\
        def main (arg : [String]) : String = showInt (f (1::[]))");
  "division by zero" >:: fails ~stderr_has:"division by zero"
    (with_fmt
       "def main (arg : [String]) : String = showInt (1 // (3 - 3))");
  "modulo by zero" >:: fails ~stderr_has:"division by zero"
    (with_fmt
       "def main (arg : [String]) : String = showInt (1 % (3 - 3))");
  "comparing functions" >:: fails ~stderr_has:"compare"
    (with_fmt
       "def adder (n : Integer) : Integer -> Integer = \\ (x : Integer) : Integer => x + n\n\
        def main (arg : [String]) : String = showBool (adder 1 == adder 2)");
]

let _ = run_test_tt_main tests
