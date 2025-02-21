open Shared
open S_exp
open Directive
open Util

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

let lf_to_bool =
  [
    Mov (Reg Rax, Imm 0);
    Setl (Reg Rax);
    Shl (Reg Rax, Imm bool_shift);
    Or (Reg Rax, Imm bool_tag);
  ]

let stack_offset (index : int) : operand = MemOffset (Reg Rsp, Imm index)

let rec compile_exp (env : int symtab) (stack_index : int) (exp : s_exp) :
    directive list =
  match exp with
  | Num n -> [ Mov (Reg Rax, operand_of_num n) ]
  | Sym "false" -> [ Mov (Reg Rax, operand_of_bool false) ]
  | Sym "true" -> [ Mov (Reg Rax, operand_of_bool true) ]
  | Sym var -> [ Mov (Reg Rax, stack_offset (Symtab.find var env)) ]
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; e ] ]; e_body ] ->
      compile_exp env stack_index e
      @ [ Mov (stack_offset stack_index, Reg Rax) ]
      @ compile_exp (Symtab.add var stack_index env) (stack_index - 8) e_body
  | Lst [ Sym "add1"; l ] ->
      let p = compile_exp env stack_index l in
      p @ [ Add (Reg Rax, operand_of_num 1) ]
  | Lst [ Sym "sub1"; l ] ->
      let p = compile_exp env stack_index l in
      p @ [ Sub (Reg Rax, operand_of_num 1) ]
  | Lst [ Sym "not"; l ] ->
      compile_exp env stack_index l
      @ [ Cmp (Reg Rax, operand_of_bool false) ]
      @ zf_to_bool
  | Lst [ Sym "zero?"; l ] ->
      compile_exp env stack_index l
      @ [ Cmp (Reg Rax, operand_of_num 0) ]
      @ zf_to_bool
  | Lst [ Sym "num?"; l ] ->
      compile_exp env stack_index l
      @ [ And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag) ]
      @ zf_to_bool
  | Lst [ Sym "if"; e_cond; e_then; e_else ] ->
      let else_label = gensym "else" in
      let cont_label = gensym "continue" in
      compile_exp env stack_index e_cond
      @ [ Cmp (Reg Rax, operand_of_bool false); Je else_label ]
      @ compile_exp env stack_index e_then
      @ [ Jmp cont_label ] @ [ Label else_label ]
      @ compile_exp env stack_index e_else
      @ [ Label cont_label ]
  | Lst [ Sym "+"; e1; e2 ] ->
      compile_binop env stack_index e1 e2 @ [ Add (Reg Rax, Reg R8) ]
  | Lst [ Sym "-"; e1; e2 ] ->
      compile_binop env stack_index e1 e2 @ [ Sub (Reg Rax, Reg R8) ]
  | Lst [ Sym "="; e1; e2 ] ->
      compile_binop env stack_index e1 e2
      @ [ Cmp (Reg Rax, Reg R8) ]
      @ zf_to_bool
  | Lst [ Sym "<"; e1; e2 ] ->
      compile_binop env stack_index e1 e2
      @ [ Cmp (Reg Rax, Reg R8) ]
      @ lf_to_bool
  | _ -> failwith "i dont know"

and compile_binop env stack_index e1 e2 =
  compile_exp env stack_index e1
  @ [ Mov (stack_offset stack_index, Reg Rax) ]
  @ compile_exp env (stack_index - 8) e2
  @ [ Mov (Reg R8, Reg Rax) ] (* we have to flip the args for sub *)
  @ [ Mov (Reg Rax, stack_offset stack_index) ]

let compile (exp : s_exp) : directive list =
  let directives = compile_exp (-8) exp in
  [ Global "lisp_entry"; Label "lisp_entry" ] @ directives @ [ Ret ]
