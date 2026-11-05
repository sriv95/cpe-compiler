(* 1. โครงสร้างของ Type ภายใน Type Checker (อ้างอิงตามเอกสารสเปกหัวข้อ 4.1 และ 4.2)[cite: 6, 7] *)
open Utils
open Ast

type ty =
  | TInt
  | TDouble
  | TBool
  | TChar
  | TUnit
  | TList of ty
  | TPair of ty * ty
  | TFun of ty * ty
  | TData of string * ty list
  | TVar of string

let rec string_of_ty (t : ty) : string =
  match t with
  | TInt -> "Integer"
  | TDouble -> "Double"
  | TBool -> "Bool"
  | TChar -> "Char"
  | TUnit -> "()"
  | TList a -> "[" ^ string_of_ty a ^ "]"
  | TPair (a, b) -> "(" ^ string_of_ty a ^ ", " ^ string_of_ty b ^ ")"
  | TFun (a, b) -> "(" ^ string_of_ty a ^ " -> " ^ string_of_ty b ^ ")"
  | TData (n, []) -> n
  | TData (n, args) ->
      "(" ^ n ^ " " ^ String.concat " " (List.map string_of_ty args) ^ ")"
  | TVar v -> v

(* สมมติฐานโครงสร้าง Environment สำหรับเก็บตัวแปรที่ค้นพบ *)
module SM = Map.Make(String)
type env = {
  vals : ty SM.t; (* เก็บชื่อตัวแปร/ฟังก์ชัน และชนิดข้อมูลของมัน *)
}

exception TypeError of string
let error msg = raise (TypeError msg)
let error_at pos msg = raise (TypeError (string_of_positions pos ^ ": " ^ msg))

(* 2. สร้าง Environment เริ่มต้น พร้อมยัดค่า Built-in ตามที่สเปกระบุ[cite: 1, 5, 6] *)
let initial_env () : env =
  let vals =
    SM.empty
    |> SM.add "ord" (TFun (TChar, TInt))
    |> SM.add "chr" (TFun (TInt, TChar))
    |> SM.add "True" TBool
    |> SM.add "False" TBool
    |> SM.add "error" (TFun (TList TChar, TUnit))
  in
  { vals }

(* 3. ฟังก์ชันประเมิน Type ของ Operator (บังคับ Type ซ้ายและขวาต้องเหมือนกัน)*)
let type_binop op (tl : ty) (tr : ty) : ty =
  match op with
  | "+" | "-" | "*" ->
      if tl = TInt && tr = TInt then TInt
      else if tl = TDouble && tr = TDouble then TDouble
      else if tl = TInt && tr = TDouble then TDouble
      else if tl = TDouble && tr = TInt then TDouble
      else error ("Operator " ^ op ^ " requires numeric types (Int or Double)")
  | "//" ->
      if tl = TInt && tr = TInt then TInt
      else if tl = TDouble && tr = TInt then TInt
      else error ("Operator " ^ op ^ " requires Int only")
  | "%" ->
      if tl = TInt && tr = TInt then TInt
      else if tl = TDouble && tr = TInt then TDouble
      else error ("Operator " ^ op ^ " requires Int % Int or Double % Int")
  | "/" ->
      if tl = TInt && tr = TInt then TDouble
      else if tl = TDouble && tr = TDouble then TDouble
      else if tl = TInt && tr = TDouble then TDouble
      else if tl = TDouble && tr = TInt then TDouble
      else error "Operator / requires numeric types (Int or Doulbe)"
  | "==" | "!=" ->
      if tl = tr then TBool
      else error ("Comparison operator " ^ op ^ " requires strictly matching operands")
  | "<" | "<=" | ">" | ">=" ->
      if tl = TInt && tr = TInt then TBool
      else if tl = TDouble && tr = TDouble then TBool
      else error ("Comparison operator " ^ op ^ " requires strictly matching operands with numeric type")
  | "&&" | "||" ->
      if tl = TBool && tr = TBool then TBool
      else error ("Logical operator " ^ op ^ " requires Bool operands")

  (* :: (เอาสมาชิก tl ไปต่อหน้าลิสต์ tr) *)
  | "::" ->
      (match tr with
       | TList (TVar _) -> TList tl (* กรณี x :: [] = x *)
       | TList t -> 
           if tl = t then TList tl 
           else error ("List elements must have the same type")
       | _ -> error "Right operand of :: must be a list")

  (* ++ (เอาลิสต์ tl มาต่อกับลิสต์ tr) *)
  | "++" ->
      (match tl, tr with
       | TList (TVar _), TList _ -> tr (* [] ++ list = list *)
       | TList _, TList (TVar _) -> tl (* list ++ [] = list *)
       | TList t1, TList t2 -> 
           if t1 = t2 then TList t1 
           else error "Lists in ++ must have the same type"
       | _ -> error "Operator ++ requires two lists")

  | _ -> error ("Unknown binary operator: " ^ op)


let check_main_function (env : env) =
  match SM.find_opt "main" env.vals with
  | None -> () 
  (* mty = main type *)
  | Some mty -> 
      (* สร้าง Type ของ [String] -> String (String = [Char])[cite: 5] *)
      let string_ty = TList TChar in
      let list_of_string_ty = TList string_ty in
      let expected_main_ty = TFun (list_of_string_ty, string_ty) in
      
      if mty <> expected_main_ty then
        error "main function must have exactly type [String] -> String"

let rec ty_of (te : typeexpr) : ty =
  match te with
  | TypeAtom { id = "Integer"; _ } -> TInt
  | TypeAtom { id = "Double"; _ } -> TDouble
  | TypeAtom { id = "Bool"; _ } -> TBool
  | TypeAtom { id = "Char"; _ } -> TChar
  | TypeAtom { id = "()"; _ } -> TUnit
  | TypeAtom { id = "String"; _ } -> TList TChar (* สเปกระบุว่า String = [Char][cite: 5] *)
  | ListType r -> TList (ty_of r.elem_ty)        (* จัดการ [String] *)
  | _ -> failwith "unimplemented"

let ty_of_param (p : param) : ty =
  match p with
  | UnitParam _ -> TUnit
  | TypedParam r -> ty_of r.ty

let add_params (env : env) (ps : param list) : env =
  List.fold_left (fun e p ->
      match p with
      | UnitParam _ -> e
      | TypedParam r -> { vals = SM.add r.id (ty_of r.ty) e.vals }) env ps
let fun_ty (ps : param list) (ret : typeexpr) : ty =
  List.fold_right (fun p acc -> TFun (ty_of_param p, acc)) ps (ty_of ret)
let rec tc (env : env) (e : unit expr) : ty expr =
  match e with
  | IntLit r -> IntLit { r with prop = TInt }
  | FloatLit r -> FloatLit { r with prop = TDouble }
  | BoolLit r -> BoolLit { r with prop = TBool }
  | StringLit r -> StringLit { r with prop = TList TChar }
  | NilLit r -> NilLit { r with prop = TList (TVar "a") } 
  | VarExpr r ->
      (match SM.find_opt r.id env.vals with
       | Some t -> VarExpr { r with prop = t }
       | None -> error_at r.pos ("unbound variable " ^ r.id))
  | BinOpExpr r ->
      let l = tc env r.left in
      let rt = tc env r.right in
      let tl = prop_of_expr l and tr = prop_of_expr rt in
      let t =
        try type_binop r.op tl tr
        with TypeError msg ->
          error_at r.pos (msg ^ " (found " ^ string_of_ty tl ^ ", "
                          ^ string_of_ty tr ^ ")")
      in
      BinOpExpr { r with prop = t; left = l; right = rt }
  | _ -> failwith "unimplemented"

let typecheck_defn (env: env) (d : unit defn) : ty defn =
  match d with
  | FuncDef r ->
      let fty = fun_ty r.params r.ret_ty in
      let body = tc (add_params env r.params) r.body in
      let expected = ty_of r.ret_ty and actual = prop_of_expr body in
      if expected <> actual then
        error_at r.pos
          (Printf.sprintf "body of %s: expected %s but found %s" r.id
             (string_of_ty expected) (string_of_ty actual));
      FuncDef { r with prop = fty; body }
  | DataDef _ | TypeDef _ -> failwith "unimplemented"


let build_global_env (defns : unit defn list) : env =
  let env = initial_env () in
  List.fold_left (fun curr_env def ->
    match def with
    | FuncDef r -> 
        let fty = fun_ty r.params r.ret_ty in
        { vals = SM.add r.id fty curr_env.vals }
    | _ -> curr_env (* ข้าม DataDef และ TypeDef ไปก่อนในตอนนี้ *)
  ) env defns

  
(* TODO: define your own type checking *)
let typecheck_prog(p : unit prog) : ty prog option =
  try
    match p with
    | Prog r -> 
      let global_env = build_global_env r.defns in
        
      (* 2. เรียกใช้ฟังก์ชัน check_main เพื่อดูว่า main พิมพ์ type ถูกไหม *)
      check_main_function global_env;
        
        (* 3. ส่ง global_env ไปใช้ตรวจ body ของแต่ละฟังก์ชันต่อไป *)
      let checked_defns = List.map (typecheck_defn global_env) r.defns in
      Some (Prog { r with defns = checked_defns })
  with TypeError msg -> prerr_endline msg; None

let typecheck(p : unit prog) : bool =
  match typecheck_prog p with
  | Some _ -> true
  | None -> false