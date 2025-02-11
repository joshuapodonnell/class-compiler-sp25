open Shared
open S_exp
open Directive

let num_shift = 2
let num_mask = 0b11
let num_tag = 0b00

let bool_tag = 0b0011111
let bool_shift = 7
let bool_mask = 0b1111111

let rec compile_exp (exp: s_exp): directive list = 
  match exp with 
  Num n -> [ Mov (Reg Rax, Imm (n lsl num_shift)); ] 
  | Sym "false" -> [ Mov (Reg Rax, Imm ((0 lsl bool_shift) lor bool_tag)); ] 
  | Sym "true" -> [ Mov (Reg Rax, Imm ((1 lsl bool_shift) lor bool_tag)); ] 
  | Lst [Sym "add1"; l] -> 
    let p = compile_exp l in
    p @ [ Add (Reg Rax, Imm (1 lsl num_shift))]
  | Lst [Sym "sub1"; l] -> 
    let p = compile_exp l in
    p @ [ Sub (Reg Rax, Imm (1 lsl num_shift))]
  | _ -> failwith "i dont know"

let compile (exp: s_exp): directive list =
  let directives = compile_exp exp in
  [ Global "lisp_entry"; Label "lisp_entry" ] @ directives @ [ Ret ] 