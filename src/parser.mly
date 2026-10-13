(** header section **)
%{
open Ast
%}

(** declarations section **)

(* token declarations *)
%token <int> INT_LIT
%token TRUE "true"
%token FALSE "false"
%token PLUS "+" TIMES "*"
%token OROR "||" ANDAND "&&"
%token LPAREN "(" RPAREN ")"
%token EOF

(* precedence and associativity declarations *)
%left OROR
%left ANDAND
%left PLUS
%left TIMES

(* starting nonterminal *)
%start <Ast.prog> start
%%

(** rules section **)
start:
  | p = prog; EOF { p }

prog:
  | e = expr {
      (* ห่อ expr ให้อยู่ในโครงสร้าง Prog ของ ast.ml ใหม่ *)
      Prog {
        pos = $loc;
        imports = [];
        defns = [
          FuncDef {
            pos = $loc;
            id = "main";
            params = [UnitParam { pos = $loc }];
            ret_ty = TypeAtom { pos = $loc; id = "Integer" };
            body = e;
          }
        ];
      }
    }

expr:
  | e1 = expr; "+"; e2 = expr {
      BinOpExpr {
        pos = $loc;
        op = "+";
        left = e1;
        right = e2;
      }
    }
  | e1 = expr; "*"; e2 = expr {
      BinOpExpr {
        pos = $loc;
        op = "*";
        left = e1;
        right = e2;
      }
    }
  | e1 = expr; "||"; e2 = expr {
      BinOpExpr {
        pos = $loc;
        op = "||";
        left = e1;
        right = e2;
      }
    }
  | e1 = expr; "&&"; e2 = expr {
      BinOpExpr {
        pos = $loc;
        op = "&&";
        left = e1;
        right = e2;
      }
    }
  | "true" {
      AtomExpr {
        pos = $loc;
        literal = "True";
      }
    }
  | "false" {
      AtomExpr {
        pos = $loc;
        literal = "False";
      }
    }
  | n = INT_LIT {
      AtomExpr {
        pos = $loc;
        literal = string_of_int n;
      }
    }
  | "("; e = expr; ")" { e }