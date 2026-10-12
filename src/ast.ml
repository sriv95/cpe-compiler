type pos = Lexing.position * Lexing.position

(* TODO: define your own AST *)

type prog = Prog of {
  pos : pos;
  imports : import list;
  defns : defn list;
}

and import =
  | Import of { pos : pos; id : string }
  | ImportAs of { pos : pos; id : string; ns : string }

and defn =
  | DataDef of { pos : pos; decl : typedecl; ctors : constructor list }
  | TypeDef of { pos : pos; decl : typedecl; body : typeexpr }
  | FuncDef of { pos : pos; id : string; params : param list; ret_ty : typeexpr; body : expr }

and typedecl =
  | SimpleTypeDecl of { pos : pos; id : string }
  | PolyTypeDecl of { pos : pos; id : string; args : string list }

and constructor =
  | SimpleCons of { pos : pos; id : string }
  | ParamCons of { pos : pos; id : string; args : typeexpr list }

and typeexpr =
  | TypeAtom of { pos : pos; id : string }
  | ListType of { pos : pos; elem_ty : typeexpr }
  | PairType of { pos : pos; left : typeexpr; right : typeexpr }
  | FuncType of { pos : pos; arg : typeexpr; ret : typeexpr }
  | AppType of { pos : pos; fn : typeexpr; arg : typeexpr }

and param =
  | UnitParam of { pos : pos }
  | TypedParam of { pos : pos; id : string; ty : typeexpr }

and expr =
  | AtomExpr of { pos : pos; literal : string }
  | NamespacedExpr of { pos : pos; ns : string; id : string }
  | UnOpExpr of { pos : pos; op : string; arg : expr }
  | BinOpExpr of { pos : pos; op : string; left : expr; right : expr }
  | AppExpr of { pos : pos; fn : expr; arg : expr }
  | LambdaExpr of { pos : pos; id : string; params : param list; ret_ty : typeexpr; body : expr }
  | IfExpr of { pos : pos; cond : expr; then_branch : expr; else_branch : expr }
  | LetExpr of { pos : pos; bindings : letbinding list; body : expr }
  | MatchExpr of { pos : pos; target : expr; cases : case list }

and letbinding =
  | VarBinding of { pos : pos; id : string; value : expr }
  | FuncBinding of { pos : pos; id : string; params : param list; ret_ty : typeexpr; body : expr }

and case = Case of { pos : pos; ptrn : ptrn; body : expr }

and ptrn =
  | WildcardPtrn of { pos : pos }
  | VarPtrn of { pos : pos; id : string }
  | ConsPtrn of { pos : pos; id : string; args : ptrn list }
  | PairPtrn of { pos : pos; left : ptrn; right : ptrn }
  | ListConsPtrn of { pos : pos; head : ptrn; tail : ptrn }

(* TODO: define pretty-printing *)
(* Helper function to print space-separated lists *)
let string_of_list (f : 'a -> string) (sep : string) (l : 'a list) : string =
  String.concat sep (List.map f l)

let rec string_of_prog (Prog { imports; defns; _ }) =
  let imports_str = "(" ^ string_of_list string_of_import " " imports ^ ")" in
  let defns_str = "(" ^ string_of_list string_of_defn " " defns ^ ")" in
  "(" ^ imports_str ^ " " ^ defns_str ^ ")"

and string_of_import = function
  | Import { id; _ } -> "(import " ^ id ^ ")"
  | ImportAs { id; ns; _ } -> "(import " ^ id ^ " " ^ ns ^ ")"

and string_of_defn = function
  | DataDef { decl; ctors; _ } ->
      "(data " ^ string_of_typedecl decl ^ " (" ^ string_of_list string_of_constructor " " ctors ^ "))"
  | TypeDef { decl; body; _ } ->
      "(type " ^ string_of_typedecl decl ^ " " ^ string_of_typeexpr body ^ ")"
  | FuncDef { id; params; ret_ty; body; _ } ->
      "(def " ^ id ^ " (" ^ string_of_list string_of_param " " params ^ ") "
      ^ string_of_typeexpr ret_ty ^ " " ^ string_of_expr body ^ ")"

and string_of_typedecl = function
  | SimpleTypeDecl { id; _ } -> id
  | PolyTypeDecl { id; args; _ } -> "(" ^ id ^ " " ^ String.concat " " args ^ ")"

and string_of_constructor = function
  | SimpleCons { id; _ } -> id
  | ParamCons { id; args; _ } -> "(" ^ id ^ " " ^ string_of_list string_of_typeexpr " " args ^ ")"

and string_of_typeexpr = function
  | TypeAtom { id; _ } -> id
  | ListType { elem_ty; _ } -> "([] " ^ string_of_typeexpr elem_ty ^ ")"
  | PairType { left; right; _ } -> "(, " ^ string_of_typeexpr left ^ " " ^ string_of_typeexpr right ^ ")"
  | FuncType { arg; ret; _ } -> "(-> " ^ string_of_typeexpr arg ^ " " ^ string_of_typeexpr ret ^ ")"
  | AppType { fn; arg; _ } -> "(" ^ string_of_typeexpr fn ^ " " ^ string_of_typeexpr arg ^ ")"

and string_of_param = function
  | UnitParam _ -> "()"
  | TypedParam { id; ty; _ } -> "(" ^ id ^ " " ^ string_of_typeexpr ty ^ ")"

and string_of_expr = function
  | AtomExpr { literal; _ } -> literal
  | NamespacedExpr { ns; id; _ } -> "(! " ^ ns ^ " " ^ id ^ ")"
  | UnOpExpr { op; arg; _ } -> "(" ^ op ^ " " ^ string_of_expr arg ^ ")"
  | BinOpExpr { op; left; right; _ } -> "(" ^ op ^ " " ^ string_of_expr left ^ " " ^ string_of_expr right ^ ")"
  | AppExpr { fn; arg; _ } -> "(" ^ string_of_expr fn ^ " " ^ string_of_expr arg ^ ")"
  | LambdaExpr { id; params; ret_ty; body; _ } ->
      "(\\ " ^ id ^ " (" ^ string_of_list string_of_param " " params ^ ") "
      ^ string_of_typeexpr ret_ty ^ " " ^ string_of_expr body ^ ")"
  | IfExpr { cond; then_branch; else_branch; _ } ->
      "(if " ^ string_of_expr cond ^ " " ^ string_of_expr then_branch ^ " " ^ string_of_expr else_branch ^ ")"
  | LetExpr { bindings; body; _ } ->
      "(let (" ^ string_of_list string_of_letbinding " " bindings ^ ") " ^ string_of_expr body ^ ")"
  | MatchExpr { target; cases; _ } ->
      "(match " ^ string_of_expr target ^ " (" ^ string_of_list string_of_case " " cases ^ "))"

and string_of_letbinding = function
  | VarBinding { id; value; _ } -> "(" ^ id ^ " " ^ string_of_expr value ^ ")"
  | FuncBinding { id; params; ret_ty; body; _ } ->
      "(" ^ id ^ " (" ^ string_of_list string_of_param " " params ^ ") "
      ^ string_of_typeexpr ret_ty ^ " " ^ string_of_expr body ^ ")"

and string_of_case = function
  | Case { ptrn; body; _ } -> "(-> " ^ string_of_ptrn ptrn ^ " " ^ string_of_expr body ^ ")"

and string_of_ptrn = function
  | WildcardPtrn _ -> "_"
  | VarPtrn { id; _ } -> id
  | ConsPtrn { id; args; _ } -> "(" ^ id ^ " " ^ string_of_list string_of_ptrn " " args ^ ")"
  | PairPtrn { left; right; _ } -> "(, " ^ string_of_ptrn left ^ " " ^ string_of_ptrn right ^ ")"
  | ListConsPtrn { head; tail; _ } -> "(:: " ^ string_of_ptrn head ^ " " ^ string_of_ptrn tail ^ ")"