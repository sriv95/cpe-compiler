(** header section **)
%{
open Ast
%}

(** declarations section **)

(* token declarations *)
(* literal and names*)
%token <int>    INT_LIT
%token <float>  FLOAT_LIT
%token <char>   CHAR_LIT
%token <string> STRING_LIT
%token <string> VAR        (* lowercase-initial identifier *)
%token <string> NAME       (* uppercase-initial: types and constructors *)
%token TRUE "True" FALSE "False"

(* punctuation *)
%token LPAREN "(" RPAREN ")" LBRACKET "[" RBRACKET "]"
%token COMMA "," COLON ":" EQ "=" BAR "|" UNDERSCORE "_"
%token ARROW "->" DARROW "=>" LAMBDA "\\" BANG "!"

(* operators *)
%token PLUS "+" MINUS "-" TIMES "*" DIV "/" IDIV "//" MOD "%"
%token EQEQ "==" NEQ "!=" LT "<" LE "<=" GE ">=" GT ">"
%token ANDAND "&&" OROR "||"
%token CONS "::" APPEND "++" DOT "." DOLLAR "$"

%token EOF

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
  | imps

import:
  |

defn:
  |

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
