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
    
data:
  |

type:
  |

def:
  |

typedecl:
  |

constructor:
  |

typeexpr:
  |

param:
  |

expr:
  | e1 = expr; "+"; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Add;
      left = e1;
      right = e2;
    }}
  | e1 = expr; "*"; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = Mult;
      left = e1;
      right = e2;
    }}
  | e1 = expr; "||"; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = LOr;
      left = e1;
      right = e2;
    }}
  | e1 = expr; "&&"; e2 = expr { BinExpr {
      prop = ();
      pos = $loc;
      op = LAnd;
      left = e1;
      right = e2;
    }}
  | "true" { BoolLit {
      prop = ();
      pos = $loc;
      value = true;
    }}
  | "false" { BoolLit {
      prop = ();
      pos = $loc;
      value = false;
    }}
  | n = INT_LIT { IntLit {
      prop = ();
      pos = $loc;
      value = n;
    }}
  | "("; e = expr; ")" { e }

ifexpr:
  |

letexpr:
  |

matchexpr:
  |

letbinding:
  |

case:
  |

ptrn:
  |
