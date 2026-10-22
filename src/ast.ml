type pos = Lexing.position * Lexing.position

(* TODO: define your own AST *)
type 't prog =
  | IntLit of {
      prop : 't;
      pos : pos;
      value : int;
    }
  | BoolLit of {
      prop : 't;
      pos : pos;
      value : bool;
    }
  | BinExpr of {
      prop : 't;
      pos : pos;
      op : op;
      left : 't prog;
      right : 't prog;
    }
and op =
  | Add
  | Mult
  | LOr
  | LAnd

let prop_of_prog(prog : 't prog) : 't = match prog with
  | IntLit r -> r.prop
  | BoolLit r -> r.prop
  | BinExpr r -> r.prop

(* TODO: define pretty-printing *)
let rec string_of_prog prog : string = match prog with
  | IntLit r -> "(" ^ string_of_int r.value ^ ")"
  | BoolLit r -> "(" ^ string_of_bool r.value ^ ")"
  | BinExpr r -> "("
        ^ String.concat " " [string_of_op r.op; string_of_prog r.left; string_of_prog r.right]
        ^ ")"
and string_of_op op : string = match op with
  | Add -> "+"
  | Mult -> "*"
  | LOr -> "||"
  | LAnd -> "&&"
