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
  | "+" | "-" | "*" | "//" | "%" ->
      if tl = TInt && tr = TInt then TInt
      else if tl = TDouble && tr = TDouble then TDouble
      else error ("Operator " ^ op ^ " requires strictly matching numeric types (Int/Int or Double/Double)")
  | "/" ->
      if tl = TInt && tr = TInt then TDouble (* กฎ ARITH-INT-DIV บังคับหารคืนค่า Double[cite: 12] *)
      else if tl = TDouble && tr = TDouble then TDouble
      else error "Operator / requires Int/Int or Double/Double"
  | "==" | "!=" | "<" | "<=" | ">" | ">=" ->
      if tl = tr then TBool
      else error ("Comparison operator " ^ op ^ " requires strictly matching operands")
  | "&&" | "||" ->
      if tl = TBool && tr = TBool then TBool
      else error ("Logical operator " ^ op ^ " requires Bool operands")

  | _ -> error ("Unknown binary operator: " ^ op)


(* 6. ฟังก์ชันตรวจสอบลายเซ็น (Signature) ของ main[cite: 1] *)
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

let ty_of (te : typeexpr) : ty =
  match te with
  | TypeAtom { id = "Integer"; _ } -> TInt
  | TypeAtom { id = "Double"; _ } -> TDouble
  | TypeAtom { id = "Bool"; _ } -> TBool
  | TypeAtom { id = "Char"; _ } -> TChar
  | TypeAtom { id = "()"; _ } -> TUnit
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

let typecheck_defn (d : unit defn) : ty defn =
  match d with
  | FuncDef r ->
      let fty = fun_ty r.params r.ret_ty in
      let body = tc (add_params (initial_env ()) r.params) r.body in
      let expected = ty_of r.ret_ty and actual = prop_of_expr body in
      if expected <> actual then
        error_at r.pos
          (Printf.sprintf "body of %s: expected %s but found %s" r.id
             (string_of_ty expected) (string_of_ty actual));
      FuncDef { r with prop = fty; body }
  | DataDef _ | TypeDef _ -> failwith "unimplemented"

(* TODO: define your own type checking *)
let typecheck_prog(p : unit prog) : ty prog option =
  try
    match p with
    | Prog r -> Some (Prog { r with defns = List.map typecheck_defn r.defns })
  with TypeError msg -> prerr_endline msg; None

let typecheck(p : unit prog) : bool =
  match typecheck_prog p with
  | Some _ -> true
  | None -> false
