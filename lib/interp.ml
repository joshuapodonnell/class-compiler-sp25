open S_exp
open Shared.Error
open Util

type value = Number of int | Boolean of bool

let int_of_value (v : value) : int =
  match v with Number n -> n | Boolean _ -> failwith "boolean!"

let rec interp_exp (env : value symtab) (exp : s_exp) : value =
  match exp with
  | Num n -> Number n
  | Sym "false" -> Boolean false
  | Sym "true" -> Boolean true
  | Sym var -> Symtab.find var env
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; e ] ]; e_body ] ->
      let e_value = interp_exp env e in
      interp_exp (Symtab.add var e_value env) e_body
  | Lst [ Sym "add1"; l ] -> Number (int_of_value (interp_exp env l) + 1)
  | Lst [ Sym "sub1"; l ] -> Number (int_of_value (interp_exp env l) - 1)
  | Lst [ Sym "not"; l ] ->
      if interp_exp env l = Boolean false then Boolean true else Boolean false
  | Lst [ Sym "zero?"; l ] ->
      if interp_exp env l = Number 0 then Boolean true else Boolean false
  | Lst [ Sym "num?"; l ] -> (
      match interp_exp env l with
      | Number _ -> Boolean true
      | _ -> Boolean false)
  | Lst [ Sym "if"; e_cond; e_then; e_else ] ->
      let v_cond = interp_exp env e_cond in
      if v_cond = Boolean false then interp_exp env e_else
      else interp_exp env e_then
  | Lst [ Sym "+"; e1; e2 ] ->
      Number
        (int_of_value (interp_exp env e1) + int_of_value (interp_exp env e2))
  | Lst [ Sym "-"; e1; e2 ] ->
      Number
        (int_of_value (interp_exp env e1) - int_of_value (interp_exp env e2))
  | Lst [ Sym "="; e1; e2 ] -> Boolean (interp_exp env e1 = interp_exp env e2)
  | Lst [ Sym "<"; e1; e2 ] -> (
      match (interp_exp env e1, interp_exp env e2) with
      | Number n1, Number n2 -> Boolean (n1 < n2)
      | _ -> raise (Stuck exp))
  | _ -> raise (Stuck exp)

let string_of_value (v : value) : string =
  match v with Number n -> string_of_int n | Boolean b -> string_of_bool b

let interp (program : s_exp) : string =
  interp_exp Symtab.empty program |> string_of_value
