(** header section **)
%{
open Ast
%}

(** declarations section **)

(* token declarations *)
/* Literals and Names */
%token <int>    INT_LIT
%token <float>  FLOAT_LIT
%token <char>   CHAR_LIT
%token <string> STRING_LIT
%token <string> VAR        (* lowercase-initial identifier *)
%token <string> NAME       (* uppercase-initial: types and constructors *)
%token TRUE "True" FALSE "False"

/* Keywords */
%token IMPORT "import" AS "as"
%token DATA "data" TYPE "type" DEF "def"
%token IF "if" THEN "then" ELSE "else"
%token MATCH "match" WITH "with"
%token LET "let" IN "in" AND "and"

/* Punctuation */
%token LPAREN "(" RPAREN ")" LBRACKET "[" RBRACKET "]"
%token COMMA "," COLON ":" EQ "=" BAR "|" UNDERSCORE "_"
%token ARROW "->" DARROW "=>" LAMBDA "\\" BANG "!"

/* Operators */
%token PLUS "+" MINUS "-" TIMES "*" DIV "/" IDIV "//" MOD "%"
%token EQEQ "==" NEQ "!=" LT "<" LE "<=" GE ">=" GT ">"
%token ANDAND "&&" OROR "||"
%token CONS "::" APPEND "++" DOT "." DOLLAR "$"

/* Special Tokens */
%token EOF
%token UMINUS (* Pseudo-token for unary minus precedence *)

(* precedence and associativity declarations *)
%nonassoc MATCH_END                      (* closes a match; must be below BAR *)
%nonassoc BAR
%nonassoc IN DARROW ELSE ARROW           (* let / lambda / if / match-case bodies *)
%right DOLLAR
%right OROR
%right ANDAND
%nonassoc EQEQ NEQ LT LE GE GT
%right CONS APPEND
%left PLUS MINUS
%left TIMES DIV IDIV MOD
%nonassoc UMINUS                         (* pseudo-token for prefix minus *)
%right DOT

(* starting nonterminal *)
%start <unit prog> start
%%

(** rules section **)
start:
  | prog = prog; EOF { prog }

prog:
  | imps = list(import); defs = list(defn)
    { Prog { pos = $loc; imports = imps; defns = defs}}

import:
  | "import"; f = VAR
    { Import { pos = $loc; id = f}}
  | "import"; f = VAR; "as"; n = VAR
    { ImportAs { pos = $loc; id = f; ns = n}}

defn:
  (* user defined data type *)
  | "data"; d = typedecl; "="; cs = separated_nonempty_list(BAR, constructor)
    { DataDef { pos = $loc; decl = d; ctors = cs}}
  (* type synonyms *)
  | "type"; d = typedecl; "="; t = typeexpr
    { TypeDef { pos = $loc; decl = d; body = t}}
  (* function declaration *)
  | "def"; f = VAR; ps = list(param); ":"; t = typeexpr; e = expr
    { FuncDef { prop = (); pos = $loc; id = f; params = ps; ret_ty = t; body = e}} 

typedecl:
  | n = NAME
    { SimpleTypeDecl { pos = $loc; id = n}}
  | n = NAME; vs = nonempty_list(VAR)
    { PolyTypeDecl { pos = $loc; id = n; args = vs}}

constructor:
  | n = NAME 
    { SimpleCons { pos = $loc; id = n}}
  | n = NAME; args = nonempty_list(atype)
    { ParamCons { pos = $loc; id = n; args = args}}

param:
  | "("; ")"
    { UnitParam { pos = $loc}}
  | "("; x = VAR; ":"; t = typeexpr; ")"
    { TypedParam { pos = $loc; id = x; ty = t}}

typeexpr:
  | t = btype
    { t }
  (* function type *)
  | a = btype; "->"; r = typeexpr
    { FuncType { pos = $loc; arg = a; ret = r}}

btype:
  | t = atype
    { t }
  (* function application *)
  | f = btype; a = atype
    { AppType { pos = $loc; fn = f; arg = a}}

atype:
  | n = NAME 
    { TypeAtom { pos = $loc; id = n}}
  | v = VAR
    { TypeAtom { pos = $loc; id = v}}
  | "("; ")"
    { TypeAtom { pos = $loc; id = "()"}}
  | "["; t = typeexpr; "]"
    { ListType { pos = $loc; elem_ty = t}}
  (* function parameter *)
  | "("; t = typeexpr; ")"
    { t }
  | "("; a = typeexpr; ","; b = typeexpr; ")"
    { PairType { pos = $loc; left = a; right = b}}

%inline binop:
  | "+"  { "+" }  | "-"  { "-" }  | "*"  { "*" }
  | "/"  { "/" }  | "//" { "//" } | "%"  { "%" }
  | "==" { "==" } | "!=" { "!=" }
  | "<"  { "<" }  | "<=" { "<=" } | ">=" { ">=" } | ">" { ">" }
  | "&&" { "&&" } | "||" { "||" }
  | "::" { "::" } | "++" { "++" } | "." { "." } (* fucntion application *)

expr:
  | e = app_expr
    { e }
  | l = expr; op = binop; r = expr
    { BinOpExpr { prop = (); pos = $loc; op = op; left = l; right = r}}
  (* function application operator *)
  | f = expr; "$"; x = expr; 
    { AppExpr { prop = (); pos = $loc; fn = f; arg = x}}
  (* force precedence of UMINUS so it gropus them first *)
  | "-"; e = expr %prec UMINUS
    { UnOpExpr { prop = (); pos = $loc; op = "-"; arg = e}}
  | "if"; c = expr; "then"; t = expr; "else"; e = expr
    { IfExpr { prop = (); pos = $loc; cond = c; then_branch = t; else_branch = e}}
  | "let" bs = separated_nonempty_list(AND, letbinding); "in"; e = expr
    { LetExpr { prop = (); pos = $loc; bindings = bs; body = e}}
  | "\\"; ps = nonempty_list(param); ":"; t = typeexpr; "=>"; e = expr
    { LambdaExpr { prop = (); pos = $loc; params = ps; ret_ty = t; body = e}}
  | "match"; e = expr; "with"; BAR; cs = cases %prec MATCH_END
    { MatchExpr { prop = (); pos = $loc; target = e; cases = cs}}

app_expr: 
  | a = atom;
    { a }
  | f = app_expr; x = atom;
    { AppExpr { prop = (); pos = $loc; fn = f; arg = x}}

atom:
  | n = INT_LIT
    { IntLit { prop = (); pos = $loc; value = n}}
  | f = FLOAT_LIT
    { FloatLit { prop = (); pos = $loc; value = n}}
  | c = CHAR_LIT
    { CharLit { prop = (); pos = $loc; value = c}}
  | s = STRING_LIT
    { StringLit { prop = (); pos = $loc; value = s}}
  | "True"
    { BoolLit { prop = (); pos = $loc; value = true}}
  | "False"
    { BoolLit { prop = (); pos = $loc; value = false}}
  | x = VAR
    { VarExpr { prop = (); pos = $loc; id = x}}
  | c = NAME
    { ConExpr { prop = (); pos = $loc; id = c}}
  | n = VAR; "!"; x = VAR
    { NamespaceExpr { prop = (); pos = $loc; ns = n; id = x}}
  | "("; ")"
    { UnitLit { prop = (); pos = $loc}}
  | "["; "]"
    { NilLit { prop = (); pos = $loc}}
  | "("; e = expr; ")"
    { e }
  | "("; a = expr; ","; b = expr; ")"
    { TupleExpr { prop = (); pos = $loc; left = a; right = b}}

letbinding:
  | x = VAR; "="; e = expr
    { VarBinding { prop = (); pos = $loc; id = x; value = e}}
  | f = VAR; ps = nonempty_list(param); ":"; t= typeexpr; "="; e = expr
    { FuncBinding { prop = (); pos = $loc; id = f; params = ps; ret_ty = t; body = e}}

cases:
  | c = case
    { [c] }
  | cs = cases; "|"; c = case
    { cs @ [c]}

case:
  | p = pattern; "->"; e = expr
    { Case { pos = $loc; ptrn = p; body = e}}

pattern:
  | p = cpattern
    { p }
  | h = cpattern; "::"; t = pattern
    { ListConsPtrn { prop = (); pos = $loc; head = h; tail = t}}
  
cpattern:
  | p = apattern  
    { p }
  | c = NAME; args = nonempty_list(apattern)
    { ConsPtrn { prop = (); pos = $loc; id = c; args = args}}

apattern:
  | "_"
    { WildcardPtrn { prop = (); pos = $loc;}}
  | x = VAR
    { VarPtrn { prop = (); pos = $loc; id = x}}
  | c = NAME
    { ConsPtrn { prop = (); pos = $loc; id = c; args = []}}
  | n = INT_LIT
    { IntPtrn { prop = (); pos = $loc; value = n}}
  | c = CHAR_LIT
    { CharPtrn { prop = (); pos = $loc; value = c}}
  | s = STRING_LIT
    { StringPtrn { prop = (); pos = $loc; value = s}}
  | "True"
    { BoolPtrn { prop = (); pos = $loc; value = true}}
  | "False"
    { BoolPtrn { prop = (); pos = $loc; value = false}}
  | "("; ")"
    { UnitPtrn { prop = (); pos = $loc}}
  | "["; "]"
    { NilPtrn { prop = (); pos = $loc}}
  | "("; p = pattern; ")" 
    { p }
  | "("; a = pattern; ","; b = pattern; ")"
    { PairPtrn { prop = (); pos = $loc; left = a; right = b}}
