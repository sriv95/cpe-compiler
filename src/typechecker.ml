open Ast

type ty =
  | Int
  | Double
  | Bool
  | Char
  | String
  | Unit
  | List of ty
  | Tuple of ty list
  | Fun of ty * ty
  | Custom of string * ty list
  | Var of string

let rec string_of_type (t : ty) : string =
  match t with
  | Int -> "Integer"
  | Double -> "Double"
  | Bool -> "Bool"
  | Char -> "Char"
  | String -> "String"
  | Unit -> "()"
  | List elem_ty -> "[" ^ string_of_type elem_ty ^ "]"
  | Tuple tys -> "(" ^ String.concat ", " (List.map string_of_type tys) ^ ")"
  | Fun (param_ty, ret_ty) ->
      let param_str =
        match param_ty with
        | Fun _ -> "(" ^ string_of_type param_ty ^ ")"
        | _ -> string_of_type param_ty
      in
      param_str ^ " -> " ^ string_of_type ret_ty
  | Custom (name, []) -> name
  | Custom (name, args) ->
      name ^ " " ^ String.concat " " (List.map (fun arg ->
        match arg with
        | Fun _ | Custom (_, _ :: _) -> "(" ^ string_of_type arg ^ ")"
        | _ -> string_of_type arg
      ) args)
  | Var name -> name

(* ฟังก์ชันหลักสำหรับเรียกจาก a3_typechecking.ml *)
let typecheck (_p : prog) : bool =
  true