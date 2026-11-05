(* LLVM IR code generator.

   The output is textual LLVM IR (opaque pointers), to be compiled together
   with runtime/cpe_rt.c, which documents the value representation:
   every value is an i64, either an immediate (n << 1 | 1) or the address of
   a heap block whose first word is a header (size << 8 | tag).

   Evaluation is strict.  Functions are closure-converted: every function,
   lambda and local function becomes an LLVM function taking its closure as
   first argument.  Calls to known functions with enough arguments are direct;
   all others go through the runtime's cpe_apply. *)
open Ast
open Typechecker

(* ---------- value representation ---------- *)

let tag_double = 250
let tag_closure = 251
let header ~size ~tag = (size lsl 8) lor tag
(* OCaml ints are 63 bits wide, so tagging goes through Int64 *)
let imm n = Int64.to_string (Int64.logor (Int64.shift_left (Int64.of_int n) 1) 1L)
let false_v = imm 0
let true_v = imm 1
let unit_v = imm 0
let nil_v = imm 0
let tag_cons = 1
let max_arity = 12

(* ---------- generator state ---------- *)

type gstate = {
  globals : Buffer.t;                       (* global data *)
  funcs : Buffer.t;                         (* function definitions *)
  mutable counter : int;                    (* unique ids *)
  arity : (string, int) Hashtbl.t;          (* symbol -> number of parameters *)
  ctor_wrappers : (string, unit) Hashtbl.t; (* constructors used as functions *)
}

type fstate = {
  g : gstate;
  body : Buffer.t;
  allocas : Buffer.t;                       (* kept in the entry block *)
  mutable n : int;                          (* register counter *)
  mutable label : string;                   (* current basic block *)
}

(* a local name: its operand, and the code of the function it is bound to if
   that is statically known *)
type binding = { op : string; known : (string * int) option }

type env = {
  locals : binding SM.t;
  genv : genv;
}

let gq name = "@\"" ^ name ^ "\""
let next_id g = g.counter <- g.counter + 1; g.counter

let ins st fmt =
  Printf.ksprintf (fun s -> Buffer.add_string st.body ("  " ^ s ^ "\n")) fmt

let reg st = st.n <- st.n + 1; Printf.sprintf "%%t%d" st.n

let new_label st base = Printf.sprintf "%s%d" base (next_id st.g)

let start st l =
  Buffer.add_string st.body (l ^ ":\n");
  st.label <- l

let global st fmt = Printf.ksprintf (fun s ->
    Buffer.add_string st.g.globals (s ^ "\n")) fmt

(* ---------- low-level emitters ---------- *)

let load_field st v i =
  let p = reg st and q = reg st and r = reg st in
  ins st "%s = inttoptr i64 %s to ptr" p v;
  ins st "%s = getelementptr i64, ptr %s, i64 %d" q p (i + 1);
  ins st "%s = load i64, ptr %s" r q;
  r

let store_field st v i x =
  let p = reg st and q = reg st in
  ins st "%s = inttoptr i64 %s to ptr" p v;
  ins st "%s = getelementptr i64, ptr %s, i64 %d" q p (i + 1);
  ins st "store i64 %s, ptr %s" x q

let alloc st nfields tag =
  let r = reg st in
  ins st "%s = call i64 @cpe_alloc(i64 %d, i64 %d)" r nfields tag;
  r

let alloc_block st tag (fields : string list) =
  let b = alloc st (List.length fields) tag in
  List.iteri (fun i f -> store_field st b i f) fields;
  b

let binop st op a b =
  let r = reg st in
  ins st "%s = %s i64 %s, %s" r op a b;
  r

let tag_int st raw = binop st "or" (binop st "shl" raw "1") "1"
let untag st v = binop st "ashr" v "1"

let bool_of st c =
  let r = reg st in
  ins st "%s = select i1 %s, i64 %s, i64 %s" r c true_v false_v;
  r

let unbox_double st v =
  let bits = load_field st v 0 in
  let d = reg st in
  ins st "%s = bitcast i64 %s to double" d bits;
  d

let box_double st d =
  let bits = reg st in
  ins st "%s = bitcast double %s to i64" bits d;
  let b = alloc st 1 tag_double in
  store_field st b 0 bits;
  b

let double_of_int st v =
  let d = reg st in
  ins st "%s = sitofp i64 %s to double" d (untag st v);
  d

let ptr_const sym = Printf.sprintf "ptrtoint (ptr %s to i64)" (gq sym)

(* ---------- function emission ---------- *)

let new_fstate g =
  { g; body = Buffer.create 1024; allocas = Buffer.create 64; n = 0;
    label = "start" }

(* [build] receives the function state, the closure register and the
   parameter registers and returns the result operand *)
let emit_function (g : gstate) (name : string) (nparams : int)
    (build : fstate -> string -> string list -> string) : unit =
  let st = new_fstate g in
  let params = List.init nparams (fun i -> Printf.sprintf "%%p%d" (i + 1)) in
  let result = build st "%env" params in
  ins st "ret i64 %s" result;
  let sig_ = String.concat ", " ("i64 %env" :: List.map (fun p -> "i64 " ^ p) params) in
  Buffer.add_string g.funcs (Printf.sprintf "define i64 %s(%s) {\nentry:\n" (gq name) sig_);
  Buffer.add_buffer g.funcs st.allocas;
  Buffer.add_string g.funcs "  br label %start\nstart:\n";
  Buffer.add_buffer g.funcs st.body;
  Buffer.add_string g.funcs "}\n\n"

let static_closure (g : gstate) (sym : string) (arity : int) =
  Buffer.add_string g.globals
    (Printf.sprintf
       "%s = private global [4 x i64] [i64 %d, i64 %s, i64 %d, i64 0], align 8\n"
       (gq (sym ^ ".clo")) (header ~size:3 ~tag:tag_closure)
       (ptr_const (sym ^ ".code")) arity)

let closure_ref sym = ptr_const (sym ^ ".clo")

(* ---------- free variables ---------- *)

let rec ptrn_vars (p : ty ptrn) : string list =
  match p with
  | VarPtrn r -> [r.id]
  | ConsPtrn r -> List.concat_map ptrn_vars r.args
  | PairPtrn r -> ptrn_vars r.left @ ptrn_vars r.right
  | ListConsPtrn r -> ptrn_vars r.head @ ptrn_vars r.tail
  | _ -> []

let param_names (ps : param list) =
  List.filter_map (function UnitParam _ -> None | TypedParam r -> Some r.id) ps

(* names used in [e] that are neither in [bound] nor bound inside it *)
let free_vars_in (bound0 : string list) (e : ty expr) : SS.t =
  let acc = ref SS.empty in
  let rec go bound (e : ty expr) =
    match e with
    | IntLit _ | FloatLit _ | BoolLit _ | CharLit _ | StringLit _
    | UnitLit _ | NilLit _ | ConExpr _ | NamespacedExpr _ -> ()
    | VarExpr r -> if not (SS.mem r.id bound) then acc := SS.add r.id !acc
    | TupleExpr r -> go bound r.left; go bound r.right
    | UnOpExpr r -> go bound r.arg
    | BinOpExpr r -> go bound r.left; go bound r.right
    | AppExpr r -> go bound r.fn; go bound r.arg
    | LambdaExpr r ->
        go (List.fold_right SS.add (param_names r.params) bound) r.body
    | IfExpr r -> go bound r.cond; go bound r.then_branch; go bound r.else_branch
    | LetExpr r ->
        let fnames = List.filter_map (function
            | FuncBinding f -> Some f.id | VarBinding _ -> None) r.bindings in
        let vnames = List.filter_map (function
            | VarBinding v -> Some v.id | FuncBinding _ -> None) r.bindings in
        let with_funcs = List.fold_right SS.add fnames bound in
        List.iter (function
            | VarBinding v -> go bound v.value
            | FuncBinding f ->
                go (List.fold_right SS.add (param_names f.params) with_funcs) f.body)
          r.bindings;
        go (List.fold_right SS.add vnames with_funcs) r.body
    | MatchExpr r ->
        go bound r.target;
        List.iter (fun (Case c) ->
            go (List.fold_right SS.add (ptrn_vars c.ptrn) bound) c.body) r.cases
  in
  go (SS.of_list bound0) e;
  !acc

(* ---------- expressions ---------- *)

let ty_of (e : ty expr) : ty = resolve (prop_of_expr e)

let rec flatten (e : ty expr) args =
  match e with
  | AppExpr r -> flatten r.fn (r.arg :: args)
  | _ -> (e, args)

let rec split n l =
  if n = 0 then ([], l)
  else match l with
    | x :: xs -> let a, b = split (n - 1) xs in (x :: a, b)
    | [] -> ([], [])

let find_ctor env id =
  match SM.find_opt id env.genv.ctors with
  | Some c -> c
  | None -> failwith ("codegen: unknown constructor " ^ id)

let ctor_sym (c : ctor_info) id = "ctor." ^ c.c_key ^ "." ^ id

(* a constructor with arguments used as a function value *)
let ctor_closure st (c : ctor_info) id : string =
  let sym = ctor_sym c id in
  if not (Hashtbl.mem st.g.ctor_wrappers sym) then begin
    Hashtbl.add st.g.ctor_wrappers sym ();
    let k = List.length c.c_args in
    emit_function st.g (sym ^ ".code") k (fun fst _ params ->
        alloc_block fst c.c_tag params);
    static_closure st.g sym k
  end;
  closure_ref sym

let string_literal st (s : string) : string =
  let n = String.length s in
  if n = 0 then nil_v
  else begin
    let id = next_id st.g in
    let name i = gq (Printf.sprintf "str.%d.%d" id i) in
    for i = 0 to n - 1 do
      let tail =
        if i = n - 1 then nil_v
        else Printf.sprintf "ptrtoint (ptr %s to i64)" (name (i + 1)) in
      global st "%s = private global [3 x i64] [i64 %d, i64 %s, i64 %s], align 8"
        (name i) (header ~size:2 ~tag:tag_cons) (imm (Char.code s.[i])) tail
    done;
    Printf.sprintf "ptrtoint (ptr %s to i64)" (name 0)
  end

let double_literal st (f : float) : string =
  let id = next_id st.g in
  global st "@\"dbl.%d\" = private global [2 x i64] [i64 %d, i64 %Ld], align 8"
    id (header ~size:1 ~tag:tag_double) (Int64.bits_of_float f);
  Printf.sprintf "ptrtoint (ptr @\"dbl.%d\" to i64)" id

let global_sym env (e : ty expr) : string option =
  match e with
  | VarExpr r ->
      (match SM.find_opt r.id env.locals with
       | Some _ -> None
       | None -> Option.map (fun (s : scheme) -> s.sym) (SM.find_opt r.id env.genv.vals))
  | NamespacedExpr r ->
      Option.bind (SM.find_opt r.ns env.genv.nss) (fun vs ->
          Option.map (fun (s : scheme) -> s.sym) (SM.find_opt r.id vs))
  | _ -> None

let rec gen_expr st (env : env) (e : ty expr) : string =
  match e with
  | IntLit r -> imm r.value
  | FloatLit r -> double_literal st r.value
  | BoolLit r -> if r.value then true_v else false_v
  | CharLit r -> imm (Char.code r.value)
  | StringLit r -> string_literal st r.value
  | UnitLit _ -> unit_v
  | NilLit _ -> nil_v
  | VarExpr _ | NamespacedExpr _ ->
      (match e with
       | VarExpr { id; _ } when SM.mem id env.locals -> (SM.find id env.locals).op
       | _ ->
           (match global_sym env e with
            | None -> failwith "codegen: unresolved name"
            | Some sym ->
                if Hashtbl.find st.g.arity sym = 0 then begin
                  let r = reg st in
                  ins st "%s = load i64, ptr %s" r (gq (sym ^ ".val"));
                  r
                end else closure_ref sym))
  | ConExpr r ->
      let c = find_ctor env r.id in
      if c.c_args = [] then imm c.c_tag else ctor_closure st c r.id
  | TupleExpr r ->
      let a = gen_expr st env r.left in
      let b = gen_expr st env r.right in
      alloc_block st 0 [a; b]
  | UnOpExpr r ->
      let a = gen_expr st env r.arg in
      (match ty_of r.arg with
       | TDouble ->
           let d = unbox_double st a in
           let n = reg st in
           ins st "%s = fneg double %s" n d;
           box_double st n
       | _ -> binop st "sub" "2" a)
  | BinOpExpr r -> gen_binop st env r.op r.left r.right
  | AppExpr _ -> gen_app st env e
  | LambdaExpr r ->
      let captured =
        captured_of env (free_vars_in (param_names r.params) r.body) in
      let name = Printf.sprintf "lam.%d" (next_id st.g) in
      lift st env name r.params r.body captured;
      let clo = alloc_closure st name (List.length r.params) captured in
      fill_captured st env clo captured;
      clo
  | IfExpr r ->
      let c = gen_expr st env r.cond in
      let t = reg st in
      ins st "%s = icmp eq i64 %s, %s" t c true_v;
      let lt = new_label st "then" and le = new_label st "else"
      and lend = new_label st "endif" in
      ins st "br i1 %s, label %%%s, label %%%s" t lt le;
      start st lt;
      let a = gen_expr st env r.then_branch in
      let la = st.label in
      ins st "br label %%%s" lend;
      start st le;
      let b = gen_expr st env r.else_branch in
      let lb = st.label in
      ins st "br label %%%s" lend;
      start st lend;
      let res = reg st in
      ins st "%s = phi i64 [ %s, %%%s ], [ %s, %%%s ]" res a la b lb;
      res
  | LetExpr r -> gen_let st env r.bindings r.body
  | MatchExpr r ->
      let v = gen_expr st env r.target in
      let lend = new_label st "mend" in
      let results = ref [] in
      List.iter (fun (Case c) ->
          let fail = new_label st "mfail" in
          let locals = gen_ptrn st env v c.ptrn fail in
          let res = gen_expr st { env with locals } c.body in
          results := (res, st.label) :: !results;
          ins st "br label %%%s" lend;
          start st fail) r.cases;
      ins st "call void @cpe_match_fail()";
      ins st "unreachable";
      start st lend;
      let res = reg st in
      ins st "%s = phi i64 %s" res
        (String.concat ", "
           (List.rev_map (fun (v, l) -> Printf.sprintf "[ %s, %%%s ]" v l) !results));
      res

(* local names a lambda must capture, in a fixed order *)
and captured_of env (fvs : SS.t) : string list =
  List.filter (fun n -> SM.mem n env.locals) (SS.elements fvs)

(* emit the code of a closure body as a separate function *)
and lift st env name (params : param list) (body : ty expr) (captured : string list) =
  emit_function st.g (name ^ ".code") (List.length params) (fun fst envreg pregs ->
      let locals = ref SM.empty in
      List.iteri (fun i c ->
          let r = load_field fst envreg (3 + i) in
          locals := SM.add c { op = r; known = (SM.find c env.locals).known } !locals)
        captured;
      List.iter2 (fun p reg ->
          match p with
          | UnitParam _ -> ()
          | TypedParam { id; _ } -> locals := SM.add id { op = reg; known = None } !locals)
        params pregs;
      (* a function can refer to a sibling or to itself through its capture;
         the known-code information was already propagated above *)
      gen_expr fst { env with locals = !locals } body)

and alloc_closure st code arity (captured : string list) : string =
  let clo = alloc st (3 + List.length captured) tag_closure in
  store_field st clo 0 (ptr_const (code ^ ".code"));
  store_field st clo 1 (string_of_int arity);
  store_field st clo 2 (string_of_int (List.length captured));
  clo

and fill_captured st env clo (captured : string list) =
  List.iteri (fun i c -> store_field st clo (3 + i) (SM.find c env.locals).op) captured

and gen_let st env bindings body =
  (* variables are computed in the outer scope *)
  let var_regs = List.filter_map (function
      | VarBinding v -> Some (v.id, gen_expr st env v.value)
      | FuncBinding _ -> None) bindings in
  (* local functions: allocate every closure first, then fill the captures,
     so that they can refer to each other and to themselves *)
  let funcs = List.filter_map (function
      | FuncBinding f ->
          Some (f.id, f.params, f.body, Printf.sprintf "lam.%d" (next_id st.g))
      | VarBinding _ -> None) bindings in
  let group = List.map (fun (id, _, _, _) -> id) funcs in
  let allocated = List.map (fun (id, params, fbody, code) ->
      let fvs = free_vars_in (param_names params) fbody in
      let captured = List.filter (fun n -> SM.mem n env.locals || List.mem n group)
          (SS.elements fvs) in
      let clo = alloc_closure st code (List.length params) captured in
      (id, params, fbody, code, captured, clo)) funcs in
  let group_env =
    List.fold_left (fun l (id, params, _, code, _, clo) ->
        SM.add id { op = clo; known = Some (code, List.length params) } l)
      env.locals allocated in
  let fenv = { env with locals = group_env } in
  List.iter (fun (_, params, fbody, code, captured, clo) ->
      lift st fenv code params fbody captured;
      fill_captured st fenv clo captured) allocated;
  let locals = List.fold_left (fun l (id, op) ->
      SM.add id { op; known = None } l) group_env var_regs in
  gen_expr st { env with locals } body

and call_args st env args = List.map (gen_expr st env) args

and generic_apply st f (args : string list) : string =
  match args with
  | [] -> f
  | _ ->
      let n = List.length args in
      let arr = Printf.sprintf "%%arr%d" (next_id st.g) in
      Buffer.add_string st.allocas (Printf.sprintf "  %s = alloca [%d x i64]\n" arr n);
      List.iteri (fun i a ->
          let p = reg st in
          ins st "%s = getelementptr i64, ptr %s, i64 %d" p arr i;
          ins st "store i64 %s, ptr %s" a p) args;
      let r = reg st in
      ins st "%s = call i64 @cpe_apply(i64 %s, i64 %d, ptr %s)" r f n arr;
      r

and direct_call st code envop (args : string list) : string =
  let r = reg st in
  ins st "%s = call i64 %s(%s)" r (gq (code ^ ".code"))
    (String.concat ", " (List.map (fun a -> "i64 " ^ a) (envop :: args)));
  r

and gen_app st env (e : ty expr) : string =
  let head, args = flatten e [] in
  let generic () =
    let f = gen_expr st env head in
    generic_apply st f (call_args st env args) in
  match head with
  | ConExpr r ->
      let c = find_ctor env r.id in
      let k = List.length c.c_args in
      if k > 0 && List.length args >= k then begin
        let first, rest = split k args in
        let vals = call_args st env first in
        let blk = alloc_block st c.c_tag vals in
        generic_apply st blk (call_args st env rest)
      end else generic ()
  | VarExpr { id; _ } when SM.mem id env.locals ->
      (match SM.find id env.locals with
       | { known = Some (code, ar); op } when List.length args >= ar ->
           let first, rest = split ar args in
           let vals = call_args st env first in
           let r = direct_call st code op vals in
           generic_apply st r (call_args st env rest)
       | _ -> generic ())
  | VarExpr _ | NamespacedExpr _ ->
      (match global_sym env head with
       | Some sym ->
           let ar = Hashtbl.find st.g.arity sym in
           if ar >= 1 && List.length args >= ar then begin
             let first, rest = split ar args in
             let vals = call_args st env first in
             let r = direct_call st sym "0" vals in
             generic_apply st r (call_args st env rest)
           end else generic ()
       | None -> generic ())
  | _ -> generic ()

and gen_binop st env op (l : ty expr) (r : ty expr) : string =
  match op with
  | "&&" | "||" ->
      let a = gen_expr st env l in
      let c = reg st in
      ins st "%s = icmp eq i64 %s, %s" c a true_v;
      let from = st.label in
      let lrhs = new_label st "rhs" and lend = new_label st "endsc" in
      if op = "&&" then ins st "br i1 %s, label %%%s, label %%%s" c lrhs lend
      else ins st "br i1 %s, label %%%s, label %%%s" c lend lrhs;
      start st lrhs;
      let b = gen_expr st env r in
      let lb = st.label in
      ins st "br label %%%s" lend;
      start st lend;
      let res = reg st in
      ins st "%s = phi i64 [ %s, %%%s ], [ %s, %%%s ]" res a from b lb;
      res
  | _ ->
      let a = gen_expr st env l in
      let b = gen_expr st env r in
      let lt = ty_of l and rt = ty_of r in
      (match op with
       | "::" -> alloc_block st tag_cons [a; b]
       | "++" ->
           let res = reg st in
           ins st "%s = call i64 @cpe_append(i64 %s, i64 %s)" res a b;
           res
       | "." ->
           let res = reg st in
           ins st "%s = call i64 @cpe_compose(i64 %s, i64 %s)" res a b;
           res
       | "+" | "-" | "*" | "/" | "//" | "%" -> gen_arith st op (lt, a) (rt, b)
       | "<" | "<=" | ">" | ">=" | "==" | "!=" -> gen_compare st op (lt, a) (rt, b)
       | _ -> failwith ("codegen: unknown operator " ^ op))

and gen_arith st op (lt, a) (rt, b) : string =
  let call f =
    let res = reg st in
    ins st "%s = call i64 @%s(i64 %s, i64 %s)" res f a b;
    res
  in
  let as_double t v = if t = TInt then double_of_int st v else unbox_double st v in
  match op, lt, rt with
  | "+", TInt, TInt -> binop st "sub" (binop st "add" a b) "1"
  | "-", TInt, TInt -> binop st "add" (binop st "sub" a b) "1"
  | "*", TInt, TInt ->
      binop st "add" (binop st "mul" (binop st "sub" a "1") (untag st b)) "1"
  | ("+" | "-" | "*" | "/"), _, _ ->
      let x = as_double lt a in
      let y = as_double rt b in
      let res = reg st in
      let instr = (match op with
          | "+" -> "fadd" | "-" -> "fsub" | "*" -> "fmul" | _ -> "fdiv") in
      ins st "%s = %s double %s, %s" res instr x y;
      box_double st res
  | "//", TInt, _ -> call "cpe_idiv"
  | "//", _, _ -> call "cpe_ddivi"
  | _, TInt, _ -> call "cpe_imod"
  | _, _, _ -> call "cpe_dmodi"

and gen_compare st op (lt, a) (rt, b) : string =
  ignore rt;
  let icond = (match op with
      | "<" -> "slt" | "<=" -> "sle" | ">" -> "sgt" | ">=" -> "sge"
      | "==" -> "eq" | _ -> "ne") in
  let fcond = (match op with
      | "<" -> "olt" | "<=" -> "ole" | ">" -> "ogt" | ">=" -> "oge"
      | "==" -> "oeq" | _ -> "une") in
  match lt with
  | TInt | TChar | TBool | TUnit ->
      let c = reg st in
      ins st "%s = icmp %s i64 %s, %s" c icond a b;
      bool_of st c
  | TDouble ->
      let x = unbox_double st a and y = unbox_double st b in
      let c = reg st in
      ins st "%s = fcmp %s double %s, %s" c fcond x y;
      bool_of st c
  | _ ->
      let res = reg st in
      ins st "%s = call i64 @cpe_eq(i64 %s, i64 %s)" res a b;
      if op = "==" then res else binop st "sub" "4" res

(* branch to [fail] unless [cond] holds; continue in a fresh block *)
and branch_unless st cond fail =
  let ok = new_label st "pok" in
  ins st "br i1 %s, label %%%s, label %%%s" cond ok fail;
  start st ok

and gen_ptrn st env (v : string) (p : ty ptrn) (fail : string) : binding SM.t =
  let eq_imm n =
    let c = reg st in
    ins st "%s = icmp eq i64 %s, %s" c v n;
    branch_unless st c fail;
    env.locals
  in
  match p with
  | WildcardPtrn _ | UnitPtrn _ -> env.locals
  | VarPtrn r -> SM.add r.id { op = v; known = None } env.locals
  | IntPtrn r -> eq_imm (imm r.value)
  | BoolPtrn r -> eq_imm (if r.value then true_v else false_v)
  | CharPtrn r -> eq_imm (imm (Char.code r.value))
  | FloatPtrn r ->
      let lit = double_literal st r.value in
      let res = reg st in
      ins st "%s = call i64 @cpe_eq(i64 %s, i64 %s)" res v lit;
      let c = reg st in
      ins st "%s = icmp eq i64 %s, %s" c res true_v;
      branch_unless st c fail;
      env.locals
  | StringPtrn r ->
      let lit = string_literal st r.value in
      let res = reg st in
      ins st "%s = call i64 @cpe_eq(i64 %s, i64 %s)" res v lit;
      let c = reg st in
      ins st "%s = icmp eq i64 %s, %s" c res true_v;
      branch_unless st c fail;
      env.locals
  | NilPtrn _ -> eq_imm nil_v
  | ListConsPtrn r ->
      let c = reg st in
      ins st "%s = icmp ne i64 %s, %s" c v nil_v;
      branch_unless st c fail;
      let h = load_field st v 0 and t = load_field st v 1 in
      let env' = { env with locals = gen_ptrn st env h r.head fail } in
      gen_ptrn st env' t r.tail fail
  | PairPtrn r ->
      let a = load_field st v 0 and b = load_field st v 1 in
      let env' = { env with locals = gen_ptrn st env a r.left fail } in
      gen_ptrn st env' b r.right fail
  | ConsPtrn r ->
      let c = find_ctor env r.id in
      if c.c_args = [] then begin
        if c.c_nctors > 1 then ignore (eq_imm (imm c.c_tag));
        env.locals
      end else begin
        if c.c_nctors > 1 then begin
          let low = binop st "and" v "1" in
          let isptr = reg st in
          ins st "%s = icmp eq i64 %s, 0" isptr low;
          branch_unless st isptr fail;
          let hdr = load_field_header st v in
          let tg = binop st "and" hdr "255" in
          let ok = reg st in
          ins st "%s = icmp eq i64 %s, %d" ok tg c.c_tag;
          branch_unless st ok fail
        end;
        let locals = ref env.locals in
        List.iteri (fun i a ->
            let f = load_field st v i in
            locals := gen_ptrn st { env with locals = !locals } f a fail) r.args;
        !locals
      end

and load_field_header st v =
  let p = reg st and h = reg st in
  ins st "%s = inttoptr i64 %s to ptr" p v;
  ins st "%s = load i64, ptr %s" h p;
  h

(* ---------- whole program ---------- *)

let prelude = {|declare i64 @cpe_alloc(i64, i64)
declare void @cpe_error(i64) noreturn
declare void @cpe_match_fail() noreturn
declare i64 @cpe_idiv(i64, i64)
declare i64 @cpe_imod(i64, i64)
declare i64 @cpe_ddivi(i64, i64)
declare i64 @cpe_dmodi(i64, i64)
declare i64 @cpe_eq(i64, i64)
declare i64 @cpe_append(i64, i64)
declare i64 @cpe_compose(i64, i64)
declare i64 @cpe_apply(i64, i64, ptr)

|}

let def_sym modid id = modid ^ "." ^ id

let generate (mods : cmodule list) : string =
  let g = { globals = Buffer.create 4096; funcs = Buffer.create 16384;
            counter = 0; arity = Hashtbl.create 64;
            ctor_wrappers = Hashtbl.create 16 } in
  (* builtins *)
  List.iter (fun (s, n) -> Hashtbl.replace g.arity s n)
    ["$ord", 1; "$chr", 1; "$error", 1];
  emit_function g "$ord.code" 1 (fun _ _ ps -> List.hd ps);
  emit_function g "$chr.code" 1 (fun _ _ ps -> List.hd ps);
  emit_function g "$error.code" 1 (fun st _ ps ->
      ins st "call void @cpe_error(i64 %s)" (List.hd ps);
      ins st "unreachable";
      start st (new_label st "dead");
      "0");
  List.iter (fun s -> static_closure g s 1) ["$ord"; "$chr"; "$error"];
  (* arities of all top-level definitions *)
  List.iter (fun m ->
      match m.cm_prog with
      | Prog p ->
          List.iter (function
              | FuncDef r ->
                  let n = List.length r.params in
                  if n > max_arity then
                    failwith (Printf.sprintf "function %s has more than %d parameters"
                                r.id max_arity);
                  Hashtbl.replace g.arity (def_sym m.cm_id r.id) n
              | _ -> ()) p.defns) mods;
  (* functions and variables *)
  List.iter (fun m ->
      match m.cm_prog with
      | Prog p ->
          let env = { locals = SM.empty; genv = m.cm_genv } in
          List.iter (function
              | FuncDef r when r.params <> [] ->
                  let sym = def_sym m.cm_id r.id in
                  emit_function g (sym ^ ".code") (List.length r.params)
                    (fun st _ pregs ->
                       let locals = ref SM.empty in
                       List.iter2 (fun p reg ->
                           match p with
                           | UnitParam _ -> ()
                           | TypedParam { id; _ } ->
                               locals := SM.add id { op = reg; known = None } !locals)
                         r.params pregs;
                       gen_expr st { env with locals = !locals } r.body);
                  static_closure g sym (List.length r.params)
              | FuncDef r ->
                  Buffer.add_string g.globals
                    (Printf.sprintf "%s = global i64 %s, align 8\n"
                       (gq (def_sym m.cm_id r.id ^ ".val")) unit_v)
              | _ -> ()) p.defns) mods;
  (* initialisation of top-level variables, in dependency order *)
  emit_function g "cpe_init" 0 (fun st _ _ ->
      List.iter (fun m ->
          match m.cm_prog with
          | Prog p ->
              let env = { locals = SM.empty; genv = m.cm_genv } in
              List.iter (function
                  | FuncDef r when r.params = [] ->
                      let v = gen_expr st env r.body in
                      ins st "store i64 %s, ptr %s" v
                        (gq (def_sym m.cm_id r.id ^ ".val"))
                  | _ -> ()) p.defns) mods;
      unit_v);
  (* entry point *)
  let root = List.nth mods (List.length mods - 1) in
  let main_sym = def_sym root.cm_id "main" in
  (match Hashtbl.find_opt g.arity main_sym with
   | Some 1 -> ()
   | _ -> failwith "the program has no main function");
  emit_function g "cpe_user_main" 1 (fun st _ ps ->
      let r = reg st in
      ins st "%s = call i64 %s(i64 0, i64 %s)" r (gq (main_sym ^ ".code")) (List.hd ps);
      r);
  prelude ^ Buffer.contents g.globals ^ "\n" ^ Buffer.contents g.funcs
