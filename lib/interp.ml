open S_exp

type value = Number of int | Boolean of bool

let int_of_value (v: value): int =
  match v with
  Number n -> n
  | Boolean _ -> failwith "boolean!"

let string_of_value (v: value): string = 
  match v with
  Number n -> string_of_int n
  | Boolean b -> string_of_bool b

let rec interp_exp (exp: s_exp): value = 
  match exp with 
  Num n -> Number n
  | Sym "false" -> Boolean false
  | Sym "true" -> Boolean true
  | Lst [Sym "add1"; l] -> Number ((int_of_value (interp_exp l)) + 1)
  | Lst [Sym "sub1"; l] -> Number ((int_of_value (interp_exp l)) - 1)
  (* not handling all the bool cases yet *)
  | _ -> failwith "i dont know"

let interp (program: s_exp): string = interp_exp program |> string_of_value