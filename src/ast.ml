type pos = Lexing.position * Lexing.position

type 't prog = Prog of {
  pos : pos;
  imports : import list;
  defns : 't defn list;
}

and import =
  | Import of { pos : pos; id : string }
  | ImportAs of { pos : pos; id : string; ns : string }

and 't defn =
  | DataDef of { pos : pos; decl : typedecl; ctors : constructor list }
  | TypeDef of { pos : pos; decl : typedecl; body : typeexpr }
  | FuncDef of { prop : 't; pos : pos; id : string; params : param list;
                 ret_ty : typeexpr; body : 't expr }

and typedecl =
  | SimpleTypeDecl of { pos : pos; id : string }
  | PolyTypeDecl of { pos : pos; id : string; args : string list }

and constructor =
  | SimpleCons of { pos : pos; id : string }
  | ParamCons of { pos : pos; id : string; args : typeexpr list }

and typeexpr =
  | TypeAtom of { pos : pos; id : string }   (* Integer, a, Tree, or "()" *)
  | ListType of { pos : pos; elem_ty : typeexpr }
  | PairType of { pos : pos; left : typeexpr; right : typeexpr }
  | FuncType of { pos : pos; arg : typeexpr; ret : typeexpr }
  | AppType of { pos : pos; fn : typeexpr; arg : typeexpr }

and param =
  | UnitParam of { pos : pos }
  | TypedParam of { pos : pos; id : string; ty : typeexpr }

and 't expr =
  | IntLit of { prop : 't; pos : pos; value : int }
  | FloatLit of { prop : 't; pos : pos; value : float }
  | BoolLit of { prop : 't; pos : pos; value : bool }
  | CharLit of { prop : 't; pos : pos; value : char }
  | StringLit of { prop : 't; pos : pos; value : string }
  | UnitLit of { prop : 't; pos : pos }
  | NilLit of { prop : 't; pos : pos }
  | VarExpr of { prop : 't; pos : pos; id : string }
  | ConExpr of { prop : 't; pos : pos; id : string }
  | NamespacedExpr of { prop : 't; pos : pos; ns : string; id : string }
  | TupleExpr of { prop : 't; pos : pos; left : 't expr; right : 't expr }
  | UnOpExpr of { prop : 't; pos : pos; op : string; arg : 't expr }
  | BinOpExpr of { prop : 't; pos : pos; op : string; left : 't expr; right : 't expr }
  | AppExpr of { prop : 't; pos : pos; fn : 't expr; arg : 't expr }
  | LambdaExpr of { prop : 't; pos : pos; params : param list;
                    ret_ty : typeexpr; body : 't expr }
  | IfExpr of { prop : 't; pos : pos; cond : 't expr;
                then_branch : 't expr; else_branch : 't expr }
  | LetExpr of { prop : 't; pos : pos; bindings : 't letbinding list; body : 't expr }
  | MatchExpr of { prop : 't; pos : pos; target : 't expr; cases : 't case list }

and 't letbinding =
  | VarBinding of { prop : 't; pos : pos; id : string; value : 't expr }
  | FuncBinding of { prop : 't; pos : pos; id : string; params : param list;
                     ret_ty : typeexpr; body : 't expr }

and 't case = Case of { pos : pos; ptrn : 't ptrn; body : 't expr }

and 't ptrn =
  | WildcardPtrn of { prop : 't; pos : pos }
  | VarPtrn of { prop : 't; pos : pos; id : string }
  | ConsPtrn of { prop : 't; pos : pos; id : string; args : 't ptrn list }
      (* constructor pattern; args = [] for a nullary constructor like Leaf *)
  | PairPtrn of { prop : 't; pos : pos; left : 't ptrn; right : 't ptrn }
  | ListConsPtrn of { prop : 't; pos : pos; head : 't ptrn; tail : 't ptrn }
  | NilPtrn of { prop : 't; pos : pos }
  | UnitPtrn of { prop : 't; pos : pos }
  | IntPtrn of { prop : 't; pos : pos; value : int }
  | BoolPtrn of { prop : 't; pos : pos; value : bool }
  | CharPtrn of { prop : 't; pos : pos; value : char }
  | StringPtrn of { prop : 't; pos : pos; value : string }

let prop_of_expr (e : 't expr) : 't = match e with
  | IntLit r -> r.prop | FloatLit r -> r.prop | BoolLit r -> r.prop
  | CharLit r -> r.prop | StringLit r -> r.prop | UnitLit r -> r.prop
  | NilLit r -> r.prop | VarExpr r -> r.prop | ConExpr r -> r.prop
  | NamespacedExpr r -> r.prop | TupleExpr r -> r.prop | UnOpExpr r -> r.prop
  | BinOpExpr r -> r.prop | AppExpr r -> r.prop | LambdaExpr r -> r.prop
  | IfExpr r -> r.prop | LetExpr r -> r.prop | MatchExpr r -> r.prop

let prop_of_ptrn (p : 't ptrn) : 't = match p with
  | WildcardPtrn r -> r.prop | VarPtrn r -> r.prop | ConsPtrn r -> r.prop
  | PairPtrn r -> r.prop | ListConsPtrn r -> r.prop | NilPtrn r -> r.prop
  | UnitPtrn r -> r.prop | IntPtrn r -> r.prop | BoolPtrn r -> r.prop
  | CharPtrn r -> r.prop | StringPtrn r -> r.prop

(* ---------- pretty-printing (S-expressions) ---------- *)
(* ---------- pretty-printing (S-expressions) ---------- *)
let s_expr_of_list (items : string list) : string =
  "(" ^ String.concat " " items ^ ")"

let rec string_of_prog (Prog r : 't prog) : string =
  s_expr_of_list [
    s_expr_of_list (List.map string_of_import r.imports);
    s_expr_of_list (List.map string_of_defn r.defns)
  ]

and string_of_import = function
  | Import r -> s_expr_of_list ["import"; r.id]
  | ImportAs r -> s_expr_of_list ["import"; r.id; r.ns]

and string_of_defn = function
  | DataDef r ->
      s_expr_of_list [
        "data";
        string_of_typedecl r.decl;
        s_expr_of_list (List.map string_of_constructor r.ctors)
      ]
  | TypeDef r ->
      s_expr_of_list ["type"; string_of_typedecl r.decl; string_of_typeexpr r.body]
  | FuncDef r ->
      s_expr_of_list [
        "def"; r.id; string_of_params r.params;
        string_of_typeexpr r.ret_ty; string_of_expr r.body
      ]

and string_of_typedecl = function
  | SimpleTypeDecl r -> r.id
  | PolyTypeDecl r -> s_expr_of_list (r.id :: r.args)

and string_of_constructor = function
  | SimpleCons r -> r.id
  | ParamCons r ->
      s_expr_of_list (r.id :: List.map string_of_typeexpr r.args)

and string_of_typeexpr = function
  | TypeAtom r -> r.id
  | ListType r -> s_expr_of_list ["[]"; string_of_typeexpr r.elem_ty]
  | PairType r ->
      s_expr_of_list [","; string_of_typeexpr r.left; string_of_typeexpr r.right]
  | FuncType r ->
      s_expr_of_list ["->"; string_of_typeexpr r.arg; string_of_typeexpr r.ret]
  | AppType r ->
      s_expr_of_list [string_of_typeexpr r.fn; string_of_typeexpr r.arg]

and string_of_param = function
  | UnitParam _ -> "()"
  | TypedParam r -> s_expr_of_list [r.id; string_of_typeexpr r.ty]

and string_of_params ps =
  s_expr_of_list (List.map string_of_param ps)

and string_of_expr = function
  | IntLit r -> string_of_int r.value
  | FloatLit r -> string_of_float r.value
  | BoolLit r -> string_of_bool r.value
  | CharLit r -> "'" ^ Char.escaped r.value ^ "'"
  | StringLit r -> "\"" ^ String.escaped r.value ^ "\""
  | UnitLit _ -> "()"
  | NilLit _ -> "[]"
  | VarExpr r -> r.id
  | ConExpr r -> r.id
  | NamespacedExpr r -> s_expr_of_list ["!"; r.ns; r.id]
  | TupleExpr r ->
      s_expr_of_list [","; string_of_expr r.left; string_of_expr r.right]
  | UnOpExpr r -> s_expr_of_list [r.op; string_of_expr r.arg]
  | BinOpExpr r ->
      s_expr_of_list [r.op; string_of_expr r.left; string_of_expr r.right]
  | AppExpr r ->
      s_expr_of_list [string_of_expr r.fn; string_of_expr r.arg]
  | LambdaExpr r ->
      s_expr_of_list [
        "\\"; string_of_params r.params;
        string_of_typeexpr r.ret_ty; string_of_expr r.body
      ]
  | IfExpr r ->
      s_expr_of_list [
        "if"; string_of_expr r.cond;
        string_of_expr r.then_branch; string_of_expr r.else_branch
      ]
  | LetExpr r ->
      s_expr_of_list [
        "let";
        s_expr_of_list (List.map string_of_binding r.bindings);
        string_of_expr r.body
      ]
  | MatchExpr r ->
      s_expr_of_list [
        "match"; string_of_expr r.target;
        s_expr_of_list (List.map string_of_case r.cases)
      ]

and string_of_binding = function
  | VarBinding r -> s_expr_of_list [r.id; string_of_expr r.value]
  | FuncBinding r ->
      s_expr_of_list [
        r.id; string_of_params r.params;
        string_of_typeexpr r.ret_ty; string_of_expr r.body
      ]

and string_of_case (Case r) =
  s_expr_of_list ["->"; string_of_ptrn r.ptrn; string_of_expr r.body]

and string_of_ptrn = function
  | WildcardPtrn _ -> "_"
  | VarPtrn r -> r.id
  | ConsPtrn r ->
      (match r.args with
       | [] -> r.id
       | args -> s_expr_of_list (r.id :: List.map string_of_ptrn args))
  | PairPtrn r ->
      s_expr_of_list [","; string_of_ptrn r.left; string_of_ptrn r.right]
  | ListConsPtrn r ->
      s_expr_of_list ["::"; string_of_ptrn r.head; string_of_ptrn r.tail]
  | NilPtrn _ -> "[]"
  | UnitPtrn _ -> "()"
  | IntPtrn r -> string_of_int r.value
  | BoolPtrn r -> string_of_bool r.value
  | CharPtrn r -> "'" ^ Char.escaped r.value ^ "'"
  | StringPtrn r -> "\"" ^ String.escaped r.value ^ "\""