open S_exp
open Shared.Error
open Util

type value = Number of int | Boolean of bool

let st = Symtab.empty

let int_of_value (v : value) : int =
  match v with Number n -> n | Boolean _ -> failwith "boolean!"

let rec interp_exp (exp : s_exp) : value =
  match exp with
  | Num n -> Number n
  | Sym "false" -> Boolean false
  | Sym "true" -> Boolean true
  | Lst [ Sym "add1"; l ] -> Number (int_of_value (interp_exp l) + 1)
  | Lst [ Sym "sub1"; l ] -> Number (int_of_value (interp_exp l) - 1)
  | Lst [ Sym "not"; l ] ->
      if interp_exp l = Boolean false then Boolean true else Boolean false
  | Lst [ Sym "zero?"; l ] ->
      if interp_exp l = Number 0 then Boolean true else Boolean false
  | Lst [ Sym "num?"; l ] -> (
      match interp_exp l with Number _ -> Boolean true | _ -> Boolean false)
  | Lst [ Sym "if"; e_cond; e_then; e_else ] ->
      let v_cond = interp_exp e_cond in
      if v_cond = Boolean false then interp_exp e_else else interp_exp e_then
  | Lst [ Sym "+"; e1; e2 ] ->
      Number (int_of_value (interp_exp e1) + int_of_value (interp_exp e2))
  | Lst [ Sym "-"; e1; e2 ] ->
      Number (int_of_value (interp_exp e1) - int_of_value (interp_exp e2))
  | Lst [ Sym "="; e1; e2 ] -> Boolean (interp_exp e1 = interp_exp e2)
  | Lst [ Sym "<"; e1; e2 ] -> (
      match (interp_exp e1, interp_exp e2) with
      | Number n1, Number n2 -> Boolean (n1 < n2)
      | _ -> raise (Stuck exp))
  | _ -> raise (Stuck exp)

let string_of_value (v : value) : string =
  match v with Number n -> string_of_int n | Boolean b -> string_of_bool b

let interp (program : s_exp) : string = interp_exp program |> string_of_value
