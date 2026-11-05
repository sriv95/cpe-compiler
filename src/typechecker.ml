(* Type checker for CPE.

   Approach: every top-level function carries a full signature, so checking is
   bidirectional-ish Hindley-Milner without generalisation of inferred types:
   - type variables written in a signature (a, b, ...) are rigid inside the
     function body and are instantiated with fresh metavariables at each use
     site (polymorphism);
   - metavariables only arise from things like [], constructors, [error] and
     polymorphic calls, and are solved by unification. *)
open Utils
open Ast

module SM = Map.Make(String)
module SS = Set.Make(String)

(* ---------- types ---------- *)

type ty =
  | TInt
  | TDouble
  | TBool
  | TChar
  | TUnit
  | TList of ty
  | TPair of ty * ty
  | TFun of ty * ty
  | TData of string * ty list      (* unique type key, type arguments *)
  | TVar of string                 (* rigid (declared) type variable *)
  | TMeta of meta ref              (* unification variable *)

and meta = Unbound of int | Link of ty

type scheme = { vars : string list; body : ty }

let mono t = { vars = []; body = t }

let counter = ref 0
let fresh () = incr counter; TMeta (ref (Unbound !counter))

let rec prune t =
  match t with
  | TMeta ({ contents = Link t' } as r) ->
      let t'' = prune t' in
      r := Link t'';
      t''
  | _ -> t

(* fully resolve all solved metavariables *)
let rec resolve t =
  match prune t with
  | TList a -> TList (resolve a)
  | TPair (a, b) -> TPair (resolve a, resolve b)
  | TFun (a, b) -> TFun (resolve a, resolve b)
  | TData (n, args) -> TData (n, List.map resolve args)
  | t -> t

let string_of_ty (t : ty) : string =
  let rec go p t =
    match prune t with
    | TInt -> "Integer"
    | TDouble -> "Double"
    | TBool -> "Bool"
    | TChar -> "Char"
    | TUnit -> "()"
    | TList (a) when prune a = TChar -> "String"
    | TList a -> "[" ^ go 0 a ^ "]"
    | TPair (a, b) -> "(" ^ go 0 a ^ ", " ^ go 0 b ^ ")"
    | TFun (a, b) ->
        let s = go 1 a ^ " -> " ^ go 0 b in
        if p > 0 then "(" ^ s ^ ")" else s
    | TData (n, []) -> n
    | TData (n, args) ->
        let s = n ^ " " ^ String.concat " " (List.map (go 2) args) in
        if p > 1 then "(" ^ s ^ ")" else s
    | TVar x -> x
    | TMeta { contents = Unbound n } -> "t" ^ string_of_int n
    | TMeta { contents = Link _ } -> assert false
  in
  go 0 t

exception TypeError of string
let error msg = raise (TypeError msg)
let error_at pos msg = raise (TypeError (string_of_positions pos ^ ": " ^ msg))

(* ---------- unification ---------- *)

exception Mismatch
exception Occurs

let rec occurs r t =
  match prune t with
  | TMeta r' -> r == r'
  | TList a -> occurs r a
  | TPair (a, b) | TFun (a, b) -> occurs r a || occurs r b
  | TData (_, args) -> List.exists (occurs r) args
  | _ -> false

let rec unify_ty a b =
  let a = prune a and b = prune b in
  match a, b with
  | TMeta r, TMeta r' when r == r' -> ()
  | TMeta r, t | t, TMeta r ->
      if occurs r t then raise Occurs else r := Link t
  | TInt, TInt | TDouble, TDouble | TBool, TBool | TChar, TChar
  | TUnit, TUnit -> ()
  | TVar x, TVar y when x = y -> ()
  | TList a, TList b -> unify_ty a b
  | TPair (a, b), TPair (c, d) | TFun (a, b), TFun (c, d) ->
      unify_ty a c; unify_ty b d
  | TData (n, xs), TData (m, ys)
    when n = m && List.length xs = List.length ys -> List.iter2 unify_ty xs ys
  | _ -> raise Mismatch

let unify_at pos ~(expected : ty) ~(found : ty) : unit =
  try unify_ty expected found with
  | Mismatch ->
      error_at pos (Printf.sprintf "type mismatch: expected %s but found %s"
                      (string_of_ty expected) (string_of_ty found))
  | Occurs ->
      error_at pos (Printf.sprintf "infinite type: %s occurs in %s"
                      (string_of_ty expected) (string_of_ty found))

(* ---------- schemes ---------- *)

let tvars_of (t : ty) : string list =
  let rec go acc t =
    match prune t with
    | TVar x -> if List.mem x acc then acc else acc @ [x]
    | TList a -> go acc a
    | TPair (a, b) | TFun (a, b) -> go (go acc a) b
    | TData (_, args) -> List.fold_left go acc args
    | _ -> acc
  in
  go [] t

let rec subst (m : (string * ty) list) t =
  match prune t with
  | TVar x -> (match List.assoc_opt x m with Some t' -> t' | None -> TVar x)
  | TList a -> TList (subst m a)
  | TPair (a, b) -> TPair (subst m a, subst m b)
  | TFun (a, b) -> TFun (subst m a, subst m b)
  | TData (n, args) -> TData (n, List.map (subst m) args)
  | t -> t

let instantiate (s : scheme) : ty =
  if s.vars = [] then s.body
  else subst (List.map (fun v -> (v, fresh ())) s.vars) s.body

let fun_of (args : ty list) (ret : ty) : ty =
  List.fold_right (fun a acc -> TFun (a, acc)) args ret

(* ---------- global environment ---------- *)

type tyentry =
  | DataT of { key : string; arity : int }
  | AliasT of { params : string list; body : ty }

type ctor_info = {
  c_key : string;               (* key of the parent type *)
  c_params : string list;       (* type parameters of the parent type *)
  c_args : ty list;             (* argument types, over [TVar c_params] *)
}

type genv = {
  types : tyentry SM.t;
  ctors : ctor_info SM.t;
  vals : scheme SM.t;           (* top-level functions and variables *)
  nss : scheme SM.t SM.t;       (* namespace -> prefixed imports *)
}

(* what a module makes visible to its importers: its own definitions *)
type modul = {
  e_types : tyentry SM.t;
  e_ctors : ctor_info SM.t;
  e_vals : scheme SM.t;
}

let builtin_genv : genv =
  let atom t = AliasT { params = []; body = t } in
  { types = SM.empty
            |> SM.add "Integer" (atom TInt)
            |> SM.add "Double" (atom TDouble)
            |> SM.add "Bool" (atom TBool)
            |> SM.add "Char" (atom TChar)
            |> SM.add "String" (atom (TList TChar));
    ctors = SM.empty;
    vals = SM.empty
           |> SM.add "ord" (mono (TFun (TChar, TInt)))
           |> SM.add "chr" (mono (TFun (TInt, TChar)))
           |> SM.add "error"
                { vars = ["a"]; body = TFun (TList TChar, TVar "a") };
    nss = SM.empty }

(* ---------- type expressions ---------- *)

let is_lower id = id <> "" && (match id.[0] with 'a'..'z' | '_' -> true | _ -> false)

(* [lookup] resolves a type name; [tv_ok] vets type variable names *)
let convert_type ~(lookup : pos -> string -> tyentry)
    ~(tv_ok : pos -> string -> unit) (te : typeexpr) : ty =
  let apply pos id args =
    match lookup pos id with
    | DataT { key; arity } ->
        if arity <> List.length args then
          error_at pos (Printf.sprintf "type %s expects %d argument(s) but is given %d"
                          id arity (List.length args));
        TData (key, args)
    | AliasT { params; body } ->
        if List.length params <> List.length args then
          error_at pos (Printf.sprintf "type synonym %s expects %d argument(s) but is given %d"
                          id (List.length params) (List.length args));
        subst (List.combine params args) body
  in
  let rec flatten te args =
    match te with
    | AppType { fn; arg; _ } -> flatten fn (arg :: args)
    | _ -> (te, args)
  in
  let rec go te =
    match te with
    | TypeAtom { id = "()"; _ } -> TUnit
    | TypeAtom { id; pos } when is_lower id -> tv_ok pos id; TVar id
    | TypeAtom { id; pos } -> apply pos id []
    | ListType { elem_ty; _ } -> TList (go elem_ty)
    | PairType { left; right; _ } -> TPair (go left, go right)
    | FuncType { arg; ret; _ } -> TFun (go arg, go ret)
    | AppType { pos; _ } ->
        (match flatten te [] with
         | TypeAtom { id; _ }, args when not (is_lower id) && id <> "()" ->
             apply pos id (List.map go args)
         | _ -> error_at pos "invalid type application")
  in
  go te

(* ---------- expression environment ---------- *)

type env = {
  g : genv;
  locals : scheme SM.t;
  tvs : SS.t;                   (* type variables in scope (rigid) *)
  own : SS.t;                   (* top-level names defined in this module *)
  used : SS.t ref;              (* own top-level names referenced so far *)
}

let conv (env : env) (te : typeexpr) : ty =
  convert_type
    ~lookup:(fun pos n ->
        match SM.find_opt n env.g.types with
        | Some e -> e
        | None -> error_at pos ("unknown type " ^ n))
    ~tv_ok:(fun _ _ -> ())
    te

let conv_param env = function
  | UnitParam _ -> TUnit
  | TypedParam { ty; _ } -> conv env ty

let check_dup_params pos (ps : param list) =
  ignore (List.fold_left (fun seen p ->
      match p with
      | UnitParam _ -> seen
      | TypedParam { id; pos = ppos; _ } ->
          if SS.mem id seen then
            error_at (if ppos = pos then pos else ppos)
              ("duplicate parameter " ^ id);
          SS.add id seen) SS.empty ps)

(* parameter types, return type and full function type *)
let sig_of env pos (params : param list) (ret_ty : typeexpr) =
  check_dup_params pos params;
  let ptys = List.map (conv_param env) params in
  let rty = conv env ret_ty in
  (ptys, rty, fun_of ptys rty)

let bind_params env (params : param list) (ptys : ty list) : env =
  let locals = List.fold_left2 (fun l p t ->
      match p with
      | UnitParam _ -> l
      | TypedParam { id; _ } -> SM.add id (mono t) l) env.locals params ptys
  in
  { env with locals }

let add_tvars env (t : ty) : env =
  { env with tvs = List.fold_left (fun s v -> SS.add v s) env.tvs (tvars_of t) }

(* ---------- operators ---------- *)

(* give still-unknown operands of a numeric operator a concrete type *)
let settle_numeric pos tl tr =
  let numeric t = (match prune t with TInt | TDouble -> true | _ -> false) in
  (match prune tl, prune tr with
   | TMeta _, TMeta _ ->
       unify_at pos ~expected:TInt ~found:tl;
       unify_at pos ~expected:TInt ~found:tr
   | TMeta _, _ when numeric tr -> unify_at pos ~expected:tr ~found:tl
   | _, TMeta _ when numeric tl -> unify_at pos ~expected:tl ~found:tr
   | _ -> ());
  (prune tl, prune tr)

let type_binop pos op (tl : ty) (tr : ty) : ty =
  let bad () =
    error_at pos (Printf.sprintf "operator %s cannot be applied to %s and %s"
                    op (string_of_ty tl) (string_of_ty tr))
  in
  match op with
  | "+" | "-" | "*" ->
      (match settle_numeric pos tl tr with
       | TInt, TInt -> TInt
       | (TInt | TDouble), (TInt | TDouble) -> TDouble
       | _ -> bad ())
  | "/" ->
      (match settle_numeric pos tl tr with
       | (TInt | TDouble), (TInt | TDouble) -> TDouble
       | _ -> bad ())
  | "//" ->
      (match settle_numeric pos tl tr with
       | TInt, TInt | TDouble, TInt -> TInt
       | _ -> bad ())
  | "%" ->
      (match settle_numeric pos tl tr with
       | TInt, TInt -> TInt
       | TDouble, TInt -> TDouble
       | _ -> bad ())
  | "<" | "<=" | ">=" | ">" ->
      (match settle_numeric pos tl tr with
       | TInt, TInt | TDouble, TDouble -> TBool
       | _ -> bad ())
  | "==" | "!=" ->
      unify_at pos ~expected:tl ~found:tr; TBool
  | "&&" | "||" ->
      unify_at pos ~expected:TBool ~found:tl;
      unify_at pos ~expected:TBool ~found:tr;
      TBool
  | "::" ->
      unify_at pos ~expected:(TList tl) ~found:tr; tr
  | "++" ->
      unify_at pos ~expected:(TList (fresh ())) ~found:tl;
      unify_at pos ~expected:tl ~found:tr;
      tl
  | "." ->
      (* tl . tr  =  tl after tr *)
      let a = fresh () and b = fresh () and c = fresh () in
      unify_at pos ~expected:(TFun (b, c)) ~found:tl;
      unify_at pos ~expected:(TFun (a, b)) ~found:tr;
      TFun (a, c)
  | _ -> error_at pos ("unknown binary operator " ^ op)

(* ---------- variables ---------- *)

let lookup_var env pos id : ty =
  match SM.find_opt id env.locals with
  | Some s -> instantiate s
  | None ->
      (match SM.find_opt id env.g.vals with
       | Some s ->
           if SS.mem id env.own then env.used := SS.add id !(env.used);
           instantiate s
       | None -> error_at pos ("unbound variable " ^ id))

let lookup_ns env pos ns id : ty =
  match SM.find_opt ns env.g.nss with
  | None -> error_at pos ("unknown namespace " ^ ns)
  | Some vs ->
      (match SM.find_opt id vs with
       | Some s -> instantiate s
       | None -> error_at pos (Printf.sprintf "%s is not defined in namespace %s" id ns))

let find_ctor env pos id : ctor_info =
  match SM.find_opt id env.g.ctors with
  | Some c -> c
  | None -> error_at pos ("unknown constructor " ^ id)

(* instantiated argument types and result type of a constructor *)
let instantiate_ctor (c : ctor_info) : ty list * ty =
  let m = List.map (fun v -> (v, fresh ())) c.c_params in
  (List.map (subst m) c.c_args, TData (c.c_key, List.map snd m))

(* ---------- patterns ---------- *)

let rec infer_ptrn (env : env) (binds : ty SM.t ref) (expected : ty)
    (p : unit ptrn) : ty ptrn =
  let ret pos t = unify_at pos ~expected ~found:t; t in
  match p with
  | WildcardPtrn r -> WildcardPtrn { r with prop = expected }
  | VarPtrn r ->
      if SM.mem r.id !binds then
        error_at r.pos ("variable " ^ r.id ^ " is bound more than once in a pattern");
      binds := SM.add r.id expected !binds;
      VarPtrn { r with prop = expected }
  | ConsPtrn r ->
      let c = find_ctor env r.pos r.id in
      if List.length c.c_args <> List.length r.args then
        error_at r.pos
          (Printf.sprintf "constructor %s expects %d argument(s) but pattern has %d"
             r.id (List.length c.c_args) (List.length r.args));
      let arg_tys, res = instantiate_ctor c in
      let t = ret r.pos res in
      let args = List.map2 (fun ty a -> infer_ptrn env binds ty a) arg_tys r.args in
      ConsPtrn { r with prop = t; args }
  | PairPtrn r ->
      let a = fresh () and b = fresh () in
      let t = ret r.pos (TPair (a, b)) in
      let left = infer_ptrn env binds a r.left in
      let right = infer_ptrn env binds b r.right in
      PairPtrn { r with prop = t; left; right }
  | ListConsPtrn r ->
      let a = fresh () in
      let t = ret r.pos (TList a) in
      let head = infer_ptrn env binds a r.head in
      let tail = infer_ptrn env binds t r.tail in
      ListConsPtrn { r with prop = t; head; tail }
  | NilPtrn r -> NilPtrn { r with prop = ret r.pos (TList (fresh ())) }
  | UnitPtrn r -> UnitPtrn { r with prop = ret r.pos TUnit }
  | IntPtrn r -> IntPtrn { r with prop = ret r.pos TInt }
  | BoolPtrn r -> BoolPtrn { r with prop = ret r.pos TBool }
  | CharPtrn r -> CharPtrn { r with prop = ret r.pos TChar }
  | StringPtrn r -> StringPtrn { r with prop = ret r.pos (TList TChar) }

(* ---------- expressions ---------- *)

let rec infer (env : env) (e : unit expr) : ty expr =
  match e with
  | IntLit r -> IntLit { r with prop = TInt }
  | FloatLit r -> FloatLit { r with prop = TDouble }
  | BoolLit r -> BoolLit { r with prop = TBool }
  | CharLit r -> CharLit { r with prop = TChar }
  | StringLit r -> StringLit { r with prop = TList TChar }
  | UnitLit r -> UnitLit { r with prop = TUnit }
  | NilLit r -> NilLit { r with prop = TList (fresh ()) }
  | VarExpr r -> VarExpr { r with prop = lookup_var env r.pos r.id }
  | ConExpr r ->
      let args, res = instantiate_ctor (find_ctor env r.pos r.id) in
      ConExpr { r with prop = fun_of args res }
  | NamespacedExpr r ->
      NamespacedExpr { r with prop = lookup_ns env r.pos r.ns r.id }
  | TupleExpr r ->
      let l = infer env r.left in
      let rt = infer env r.right in
      TupleExpr { r with prop = TPair (prop_of_expr l, prop_of_expr rt);
                         left = l; right = rt }
  | UnOpExpr r ->
      let a = infer env r.arg in
      let t = prop_of_expr a in
      (match r.op, prune t with
       | "-", TMeta _ -> unify_at r.pos ~expected:TInt ~found:t
       | "-", (TInt | TDouble) -> ()
       | "-", _ ->
           error_at r.pos ("unary - cannot be applied to " ^ string_of_ty t)
       | op, _ -> error_at r.pos ("unknown unary operator " ^ op));
      UnOpExpr { r with prop = prune t; arg = a }
  | BinOpExpr r ->
      let l = infer env r.left in
      let rt = infer env r.right in
      let t = type_binop r.pos r.op (prop_of_expr l) (prop_of_expr rt) in
      BinOpExpr { r with prop = t; left = l; right = rt }
  | AppExpr r ->
      let f = infer env r.fn in
      let a = infer env r.arg in
      let at = prop_of_expr a in
      let res =
        match prune (prop_of_expr f) with
        | TFun (pa, pr) ->
            unify_at (pos_of_expr r.arg) ~expected:pa ~found:at;
            pr
        | TMeta _ as ft ->
            let res = fresh () in
            unify_at r.pos ~expected:(TFun (at, res)) ~found:ft;
            res
        | ft ->
            error_at r.pos ("cannot apply a value of non-function type "
                            ^ string_of_ty ft)
      in
      AppExpr { r with prop = res; fn = f; arg = a }
  | LambdaExpr r ->
      let ptys, rty, fty = sig_of env r.pos r.params r.ret_ty in
      let env' = bind_params (add_tvars env fty) r.params ptys in
      let body = infer env' r.body in
      unify_at (pos_of_expr r.body) ~expected:rty ~found:(prop_of_expr body);
      LambdaExpr { r with prop = fty; body }
  | IfExpr r ->
      let c = infer env r.cond in
      unify_at (pos_of_expr r.cond) ~expected:TBool ~found:(prop_of_expr c);
      let t = infer env r.then_branch in
      let f = infer env r.else_branch in
      unify_at (pos_of_expr r.else_branch)
        ~expected:(prop_of_expr t) ~found:(prop_of_expr f);
      IfExpr { r with prop = prop_of_expr t; cond = c;
                      then_branch = t; else_branch = f }
  | LetExpr r -> infer_let env r.pos r.bindings r.body e
  | MatchExpr r ->
      let tgt = infer env r.target in
      let tt = prop_of_expr tgt in
      let res = fresh () in
      let cases = List.map (fun (Case c) ->
          let binds = ref SM.empty in
          let ptrn = infer_ptrn env binds tt c.ptrn in
          let locals = SM.fold (fun k t l -> SM.add k (mono t) l) !binds env.locals in
          let body = infer { env with locals } c.body in
          unify_at (pos_of_expr c.body) ~expected:res ~found:(prop_of_expr body);
          Case { c with ptrn; body }) r.cases
      in
      MatchExpr { r with prop = res; target = tgt; cases }

and infer_let env pos bindings body (_orig : unit expr) : ty expr =
  (* duplicate names *)
  ignore (List.fold_left (fun seen b ->
      let id, bpos = (match b with
          | VarBinding v -> (v.id, v.pos)
          | FuncBinding f -> (f.id, f.pos)) in
      if SS.mem id seen then
        error_at bpos ("duplicate binding of " ^ id ^ " in let");
      SS.add id seen) SS.empty bindings);
  (* signatures of local functions *)
  let sigs = List.filter_map (function
      | FuncBinding f -> Some (f.id, sig_of env f.pos f.params f.ret_ty)
      | VarBinding _ -> None) bindings in
  (* functions see themselves and each other, monomorphically *)
  let env_f =
    { env with locals = List.fold_left (fun l (id, (_, _, fty)) ->
          SM.add id (mono fty) l) env.locals sigs } in
  let bindings' = List.map (function
      | VarBinding v ->
          (* variables cannot see earlier bindings of the same let *)
          let value = infer env v.value in
          VarBinding { v with prop = prop_of_expr value; value }
      | FuncBinding f ->
          let ptys, rty, fty = List.assoc f.id sigs in
          let benv = bind_params (add_tvars env_f fty) f.params ptys in
          let fbody = infer benv f.body in
          unify_at (pos_of_expr f.body) ~expected:rty ~found:(prop_of_expr fbody);
          FuncBinding { f with prop = fty; body = fbody }) bindings in
  (* in the body, local functions are generalised over their own type vars *)
  let locals = List.fold_left (fun l b ->
      match b with
      | VarBinding v -> SM.add v.id (mono v.prop) l
      | FuncBinding f ->
          let vars = List.filter (fun x -> not (SS.mem x env.tvs)) (tvars_of f.prop) in
          SM.add f.id { vars; body = f.prop } l) env.locals bindings' in
  let body' = infer { env with locals } body in
  LetExpr { prop = prop_of_expr body'; pos; bindings = bindings'; body = body' }

(* ---------- top level ---------- *)

type loader = string -> unit prog option

let dup_check what seen name pos =
  if SS.mem name seen then error_at pos ("duplicate " ^ what ^ " " ^ name)

(* register data types and type synonyms, then constructors *)
let declare_types (g : genv) (prefix : string) (defns : unit defn list)
  : genv * SS.t * SS.t =
  let decl_of = function
    | SimpleTypeDecl { id; pos } -> (id, [], pos)
    | PolyTypeDecl { id; args; pos } -> (id, args, pos)
  in
  let types = ref g.types in
  let pending : (string, string list * typeexpr * pos) Hashtbl.t = Hashtbl.create 8 in
  let own_types = ref SS.empty in
  let data = ref [] in
  (* names, arities, parameters *)
  List.iter (fun d ->
      let register (decl : typedecl) =
        let name, params, pos = decl_of decl in
        if SM.mem name !types || Hashtbl.mem pending name then
          error_at pos ("duplicate type name " ^ name);
        ignore (List.fold_left (fun seen v ->
            dup_check "type parameter" seen v pos; SS.add v seen) SS.empty params);
        own_types := SS.add name !own_types;
        (name, params, pos)
      in
      match d with
      | DataDef { decl; ctors; _ } ->
          let name, params, pos = register decl in
          types := SM.add name
              (DataT { key = prefix ^ name; arity = List.length params }) !types;
          data := (name, params, pos, ctors) :: !data
      | TypeDef { decl; body; _ } ->
          let name, params, pos = register decl in
          Hashtbl.add pending name (params, body, pos)
      | FuncDef _ -> ()) defns;
  (* synonyms, resolved on demand so that order does not matter *)
  let visiting = ref [] in
  let rec lookup pos n =
    match SM.find_opt n !types with
    | Some e -> e
    | None ->
        (match Hashtbl.find_opt pending n with
         | None -> error_at pos ("unknown type " ^ n)
         | Some (params, body, dpos) ->
             if List.mem n !visiting then
               error_at dpos ("cyclic type synonym " ^ n);
             visiting := n :: !visiting;
             let tv_ok p v =
               if not (List.mem v params) then
                 error_at p (Printf.sprintf
                               "type variable %s is not a parameter of synonym %s" v n)
             in
             let t = convert_type ~lookup ~tv_ok body in
             visiting := List.tl !visiting;
             let entry = AliasT { params; body = t } in
             Hashtbl.remove pending n;
             types := SM.add n entry !types;
             entry)
  in
  Hashtbl.iter (fun n (_, _, pos) -> ignore (lookup pos n))
    (Hashtbl.copy pending);
  (* constructors *)
  let ctors = ref g.ctors in
  let own_ctors = ref SS.empty in
  List.iter (fun (name, params, _, cs) ->
      List.iter (fun c ->
          let cname, cargs, cpos = (match c with
              | SimpleCons { id; pos } -> (id, [], pos)
              | ParamCons { id; args; pos } -> (id, args, pos)) in
          if SM.mem cname !ctors then
            error_at cpos ("duplicate constructor name " ^ cname);
          let tv_ok p v =
            if not (List.mem v params) then
              error_at p (Printf.sprintf
                            "type variable %s is not declared by type %s" v name)
          in
          let c_args = List.map (convert_type ~lookup ~tv_ok) cargs in
          ctors := SM.add cname
              { c_key = prefix ^ name; c_params = params; c_args } !ctors;
          own_ctors := SS.add cname !own_ctors) cs) (List.rev !data);
  ({ g with types = !types; ctors = !ctors }, !own_types, !own_ctors)

(* merge an imported module's definitions into the top-level namespace *)
let merge_unprefixed (g : genv) (m : modul) pos : genv =
  let merge what a b =
    SM.union (fun n _ _ -> error_at pos ("duplicate " ^ what ^ " " ^ n ^ " in import")) a b
  in
  { g with types = merge "type name" g.types m.e_types;
           ctors = merge "constructor name" g.ctors m.e_ctors;
           vals = merge "definition" g.vals m.e_vals }

(* imported under a namespace: only values, referenced as ns!name *)
let merge_prefixed (g : genv) ns (m : modul) pos : genv =
  let existing = Option.value (SM.find_opt ns g.nss) ~default:SM.empty in
  let merged =
    SM.union (fun n _ _ ->
        error_at pos ("duplicate definition " ^ n ^ " in namespace " ^ ns))
      existing m.e_vals in
  { g with nss = SM.add ns merged g.nss }

type fitem = {
  f_id : string;
  f_pos : pos;
  f_params : param list;
  f_ret_ty : typeexpr;
  f_body : unit expr;
  f_ptys : ty list;
  f_rty : ty;
  f_fty : ty;
}

let main_ty = TFun (TList (TList TChar), TList TChar)

let rec check_module ~(load : loader) ~(cache : (string, modul) Hashtbl.t)
    ~(stack : string list) ~(prefix : string) (Prog p : unit prog)
  : modul * ty prog =
  (* imports *)
  let load_module pos id =
    if List.mem id stack then
      error_at pos ("cyclic import of " ^ id);
    match Hashtbl.find_opt cache id with
    | Some m -> m
    | None ->
        (match load id with
         | None -> error_at pos ("cannot import " ^ id)
         | Some imported ->
             let m, _ =
               check_module ~load ~cache ~stack:(id :: stack)
                 ~prefix:(id ^ ".") imported in
             Hashtbl.add cache id m;
             m)
  in
  let g = List.fold_left (fun g imp ->
      match imp with
      | Import { pos; id } -> merge_unprefixed g (load_module pos id) pos
      | ImportAs { pos; id; ns } -> merge_prefixed g ns (load_module pos id) pos)
      builtin_genv p.imports in
  (* types and constructors *)
  let g, own_types, own_ctors = declare_types g prefix p.defns in
  (* function and variable signatures *)
  let env0 = { g; locals = SM.empty; tvs = SS.empty; own = SS.empty;
               used = ref SS.empty } in
  let g, own_vals, items =
    List.fold_left (fun (g, own, items) d ->
        match d with
        | FuncDef r ->
            if SM.mem r.id g.vals then
              error_at r.pos ("duplicate definition of " ^ r.id);
            let ptys, rty, fty = sig_of { env0 with g } r.pos r.params r.ret_ty in
            let sc = { vars = tvars_of fty; body = fty } in
            ({ g with vals = SM.add r.id sc g.vals }, SS.add r.id own,
             { f_id = r.id; f_pos = r.pos; f_params = r.params;
               f_ret_ty = r.ret_ty; f_body = r.body; f_ptys = ptys;
               f_rty = rty; f_fty = fty } :: items)
        | DataDef _ | TypeDef _ -> (g, own, items)) (g, SS.empty, []) p.defns in
  let items = List.rev items in
  (* function bodies *)
  let refs : (string, SS.t) Hashtbl.t = Hashtbl.create 16 in
  let typed = Hashtbl.create 16 in
  List.iter (fun f ->
      let env = { g; locals = SM.empty; tvs = SS.empty; own = own_vals;
                  used = ref SS.empty } in
      let env = bind_params (add_tvars env f.f_fty) f.f_params f.f_ptys in
      let body = infer env f.f_body in
      unify_at (pos_of_expr f.f_body) ~expected:f.f_rty ~found:(prop_of_expr body);
      Hashtbl.replace refs f.f_id !(env.used);
      Hashtbl.replace typed f.f_id
        (FuncDef { prop = f.f_fty; pos = f.f_pos; id = f.f_id;
                   params = f.f_params; ret_ty = f.f_ret_ty; body })) items;
  (* top-level variables: no forward or circular references, even through
     functions *)
  let index = List.mapi (fun i f -> (f.f_id, i)) items in
  let var_index = List.fold_left (fun m f ->
      if f.f_params = [] then SM.add f.f_id (List.assoc f.f_id index) m else m)
      SM.empty items in
  List.iteri (fun i r ->
      if r.f_params = [] then begin
        let visited = Hashtbl.create 16 in
        let rec dfs n =
          if not (Hashtbl.mem visited n) then begin
            Hashtbl.add visited n ();
            SS.iter (fun m ->
                (match SM.find_opt m var_index with
                 | Some j when j >= i ->
                     error_at r.f_pos
                       (if m = r.f_id then
                          Printf.sprintf "variable %s refers to itself" r.f_id
                        else
                          Printf.sprintf "variable %s refers to %s, which is defined later"
                            r.f_id m)
                 | _ -> ());
                dfs m)
              (Option.value (Hashtbl.find_opt refs n) ~default:SS.empty)
          end
        in
        dfs r.f_id
      end) items;
  (* main *)
  List.iter (fun f ->
      if f.f_id = "main" && resolve f.f_fty <> main_ty then
        error_at f.f_pos
          (Printf.sprintf "main must have type [String] -> String but has type %s"
             (string_of_ty f.f_fty))) items;
  (* result *)
  let defns = List.map (fun d ->
      match d with
      | FuncDef r -> Hashtbl.find typed r.id
      | DataDef r -> DataDef { pos = r.pos; decl = r.decl; ctors = r.ctors }
      | TypeDef r -> TypeDef { pos = r.pos; decl = r.decl; body = r.body }) p.defns in
  let only names m = SM.filter (fun n _ -> SS.mem n names) m in
  ({ e_types = only own_types g.types;
     e_ctors = only own_ctors g.ctors;
     e_vals = only own_vals g.vals },
   Prog { pos = p.pos; imports = p.imports; defns })

(* ---------- entry points ---------- *)

let typecheck_result ?(load : loader = fun _ -> None) (p : unit prog)
  : (ty prog, string) result =
  try
    let _, typed =
      check_module ~load ~cache:(Hashtbl.create 8) ~stack:[] ~prefix:"" p in
    Ok typed
  with TypeError msg -> Error msg

let typecheck_prog ?load (p : unit prog) : ty prog option =
  match typecheck_result ?load p with
  | Ok typed -> Some typed
  | Error msg -> prerr_endline msg; None

let typecheck ?load (p : unit prog) : bool =
  match typecheck_prog ?load p with
  | Some _ -> true
  | None -> false
