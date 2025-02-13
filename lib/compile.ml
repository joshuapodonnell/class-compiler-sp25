open Shared
open S_exp
open Directive

let num_shift = 2
let num_mask = 0b11
let num_tag = 0b00
let bool_tag = 0b0011111
let bool_shift = 7
let bool_mask = 0b1111111

let operand_of_bool (b : bool) : operand =
  Imm (((if b then 1 else 0) lsl bool_shift) lor bool_tag)

let operand_of_num (n : int) : operand = Imm ((n lsl num_shift) lor num_tag)

let zf_to_bool =
  [
    Mov (Reg Rax, Imm 0);
    Setz (Reg Rax);
    Shl (Reg Rax, Imm bool_shift);
    Or (Reg Rax, Imm bool_tag);
  ]

let gensym : string -> string =
  let counter = ref 0 in
  fun s ->
    let symbol = Printf.sprintf "%s__%d" s !counter in
    counter := !counter + 1;
    symbol

let rec compile_exp (exp : s_exp) : directive list =
  match exp with
  | Num n -> [ Mov (Reg Rax, operand_of_num n) ]
  | Sym "false" -> [ Mov (Reg Rax, operand_of_bool false) ]
  | Sym "true" -> [ Mov (Reg Rax, operand_of_bool true) ]
  | Lst [ Sym "add1"; l ] ->
      let p = compile_exp l in
      p @ [ Add (Reg Rax, operand_of_num 1) ]
  | Lst [ Sym "sub1"; l ] ->
      let p = compile_exp l in
      p @ [ Sub (Reg Rax, operand_of_num 1) ]
  | Lst [ Sym "not"; l ] ->
      compile_exp l @ [ Cmp (Reg Rax, operand_of_bool false) ] @ zf_to_bool
  | Lst [ Sym "zero?"; l ] ->
      compile_exp l @ [ Cmp (Reg Rax, operand_of_num 0) ] @ zf_to_bool
  | Lst [ Sym "num?"; l ] ->
      compile_exp l
      @ [ And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag) ]
      @ zf_to_bool
  | Lst [ Sym "if"; e_cond; e_then; e_else ] ->
      let else_label = gensym "else" in
      let cont_label = gensym "continue" in
      compile_exp e_cond
      @ [ Cmp (Reg Rax, operand_of_bool false); Je else_label ]
      @ compile_exp e_then @ [ Jmp cont_label ] @ [ Label else_label ]
      @ compile_exp e_else @ [ Label cont_label ]
  | Lst [ Sym "+"; e1; e2 ] ->
      compile_exp e1
      @ [ Mov (Reg R8, Reg Rax) ]
      @ compile_exp e2
      @ [ Add (Reg Rax, Reg R8) ]
  | _ -> failwith "i dont know"

let compile (exp : s_exp) : directive list =
  let directives = compile_exp exp in
  [ Global "lisp_entry"; Label "lisp_entry" ] @ directives @ [ Ret ]
