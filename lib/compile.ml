open Shared
open S_exp
open Directive
open Symtab
open Util

let num_shift = 2
let num_mask = 0b11
let num_tag = 0b00
let bool_tag = 0b0011111
let bool_shift = 7
let bool_mask = 0b1111111
let pair_tag = 0b010
let heap_shift = 3
let heap_mask = 0b111

(* sanitized a label name so the assembler doesn't yell at us *)
let function_label s =
  let nasm_char c =
    match c with
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_' | '$' | '#' | '@' | '~' | '.' | '?' ->
        c
    | _ -> '_'
  in
  Printf.sprintf "function_%s_%d" (String.map nasm_char s) (Hashtbl.hash s)

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

(* our ensure functions use register r9, so they shouldn't clobber anything *)
let ensure_type (mask : int) (tag : int) (op : operand) : directive list =
  [
    Mov (Reg R9, op); And (Reg R9, Imm mask); Cmp (Reg R9, Imm tag); Jne "error";
  ]

let ensure_num : operand -> directive list = ensure_type num_mask num_tag
let ensure_pair : operand -> directive list = ensure_type heap_mask pair_tag

let align_stack_index (stack_index : int) : int =
  if stack_index mod 16 = -8 then stack_index else stack_index - 8

let rec compile_exp defns (env : int symtab) (stack_index : int) (exp : s_exp)
    (is_tail : bool) : directive list =
  match exp with
  | Num n -> [ Mov (Reg Rax, operand_of_num n) ]
  | Sym "false" -> [ Mov (Reg Rax, operand_of_bool false) ]
  | Sym "true" -> [ Mov (Reg Rax, operand_of_bool true) ]
  | Sym var -> [ Mov (Reg Rax, stack_offset (Symtab.find var env)) ]
  | Lst (Sym f :: args) when is_defn defns f && is_tail ->
      let defn = get_defn defns f in
      if List.length args = List.length defn.args then
        let compiled_args =
          args
          |> List.mapi (fun i arg ->
                 compile_exp defns env (stack_index - (i * 8)) arg false
                 @ [ Mov (stack_offset (stack_index - (i * 8)), Reg Rax) ])
          |> List.concat
        in
        let moved_args =
          args
          |> List.mapi (fun i _ ->
                 [
                   Mov (Reg R8, stack_offset (stack_index - (i * 8)));
                   Mov (stack_offset ((i + 1) * -8), Reg R8);
                 ])
          |> List.concat
        in
        compiled_args @ moved_args @ [ Jmp (function_label defn.name) ]
      else failwith "wrong number of args"
  | Lst (Sym f :: args) when is_defn defns f ->
      let defn = get_defn defns f in
      if List.length args = List.length defn.args then
        let stack_base = align_stack_index (stack_index + 8) in
        let compiled_args =
          args
          |> List.mapi (fun i arg ->
                 compile_exp defns env (stack_base - ((i + 2) * 8)) arg false
                 @ [ Mov (stack_offset (stack_base - ((i + 2) * 8)), Reg Rax) ])
          |> List.concat
        in
        compiled_args
        @ [
            Add (Reg Rsp, Imm stack_base);
            Call (function_label defn.name);
            Sub (Reg Rsp, Imm stack_base);
          ]
      else failwith "wrong number of args"
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; e ] ]; e_body ] ->
      compile_exp defns env stack_index e false
      @ [ Mov (stack_offset stack_index, Reg Rax) ]
      @ compile_exp defns
          (Symtab.add var stack_index env)
          (stack_index - 8) e_body is_tail
  | Lst [ Sym "add1"; l ] ->
      let p = compile_exp defns env stack_index l false in
      p @ ensure_num (Reg Rax) @ [ Add (Reg Rax, operand_of_num 1) ]
  | Lst [ Sym "sub1"; l ] ->
      let p = compile_exp defns env stack_index l false in
      p @ ensure_num (Reg Rax) @ [ Sub (Reg Rax, operand_of_num 1) ]
  | Lst [ Sym "not"; l ] ->
      compile_exp defns env stack_index l false
      @ [ Cmp (Reg Rax, operand_of_bool false) ]
      @ zf_to_bool
  | Lst [ Sym "zero?"; l ] ->
      compile_exp defns env stack_index l false
      @ [ Cmp (Reg Rax, operand_of_num 0) ]
      @ zf_to_bool
  | Lst [ Sym "num?"; l ] ->
      compile_exp defns env stack_index l false
      @ [ And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag) ]
      @ zf_to_bool
  | Lst [ Sym "if"; e_cond; e_then; e_else ] ->
      let else_label = gensym "else" in
      let cont_label = gensym "continue" in
      compile_exp defns env stack_index e_cond false
      @ [ Cmp (Reg Rax, operand_of_bool false); Je else_label ]
      @ compile_exp defns env stack_index e_then is_tail
      @ [ Jmp cont_label ] @ [ Label else_label ]
      @ compile_exp defns env stack_index e_else is_tail
      @ [ Label cont_label ]
  | Lst [ Sym "+"; e1; e2 ] ->
      compile_binop defns env stack_index e1 e2
      @ ensure_num (Reg Rax) @ ensure_num (Reg R8)
      @ [ Add (Reg Rax, Reg R8) ]
  | Lst [ Sym "-"; e1; e2 ] ->
      compile_binop defns env stack_index e1 e2
      @ ensure_num (Reg Rax) @ ensure_num (Reg R8)
      @ [ Sub (Reg Rax, Reg R8) ]
  | Lst [ Sym "="; e1; e2 ] ->
      compile_binop defns env stack_index e1 e2
      @ ensure_num (Reg Rax) @ ensure_num (Reg R8)
      @ [ Cmp (Reg Rax, Reg R8) ]
      @ zf_to_bool
  | Lst [ Sym "<"; e1; e2 ] ->
      compile_binop defns env stack_index e1 e2
      @ ensure_num (Reg Rax) @ ensure_num (Reg R8)
      @ [ Cmp (Reg Rax, Reg R8) ]
      @ lf_to_bool
  | Lst [ Sym "pair"; e1; e2 ] ->
      compile_binop defns env stack_index e1 e2
      @ [
          Mov (MemOffset (Reg Rdi, Imm 0), Reg Rax);
          Mov (MemOffset (Reg Rdi, Imm 8), Reg R8);
          Mov (Reg Rax, Reg Rdi);
          Add (Reg Rdi, Imm 16);
          Or (Reg Rax, Imm pair_tag);
        ]
  | Lst [ Sym "left"; e ] ->
      compile_exp defns env stack_index e false
      @ ensure_pair (Reg Rax)
      @ [ Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag))) ]
  | Lst [ Sym "right"; e ] ->
      compile_exp defns env stack_index e false
      @ ensure_pair (Reg Rax)
      @ [ Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag + 8))) ]
  | Lst [ Sym "read-num" ] ->
      [
        Mov (stack_offset stack_index, Reg Rdi);
        Add (Reg Rsp, Imm (align_stack_index stack_index));
        Call "read_num";
        Sub (Reg Rsp, Imm (align_stack_index stack_index));
        Mov (Reg Rdi, stack_offset stack_index);
      ]
  | Lst [ Sym "newline" ] ->
      [
        Mov (stack_offset stack_index, Reg Rdi);
        Add (Reg Rsp, Imm (align_stack_index stack_index));
        Call "print_newline";
        Sub (Reg Rsp, Imm (align_stack_index stack_index));
        Mov (Reg Rdi, stack_offset stack_index);
      ]
  | Lst [ Sym "print"; e ] ->
      compile_exp defns env stack_index e false
      @ [
          Mov (stack_offset stack_index, Reg Rdi);
          Add (Reg Rsp, Imm (align_stack_index stack_index));
          Mov (Reg Rdi, Reg Rax);
          Call "print_value";
          Sub (Reg Rsp, Imm (align_stack_index stack_index));
          Mov (Reg Rdi, stack_offset stack_index);
        ]
  | Lst (Sym "do" :: exprs) ->
      List.mapi
        (fun i e ->
          compile_exp defns env stack_index e
            (if i = List.length exprs - 1 then is_tail else false))
        exprs
      |> List.concat
  | _ -> failwith "i dont know"

(* puts e1, e2 into rax, r8*)
and compile_binop defns env stack_index e1 e2 =
  compile_exp defns env stack_index e1 false
  @ [ Mov (stack_offset stack_index, Reg Rax) ]
  @ compile_exp defns env (stack_index - 8) e2 false
  @ [ Mov (Reg R8, Reg Rax) ] (* we have to flip the args for sub *)
  @ [ Mov (Reg Rax, stack_offset stack_index) ]

let compile_defn defns defn =
  let env =
    defn.args |> List.mapi (fun i arg -> (arg, -8 * (i + 1))) |> Symtab.of_list
  in
  let stack_index = -8 * (List.length defn.args + 1) in
  [ Label (function_label defn.name) ]
  @ compile_exp defns env stack_index defn.body true
  @ [ Ret ]

let compile (program : s_exp list) : directive list =
  let defns, body = defns_and_body program in
  let body_directives = compile_exp defns Symtab.empty (-8) body true in
  [
    Extern "error";
    Extern "read_num";
    Extern "print_newline";
    Extern "print_value";
    Global "lisp_entry";
    Label "lisp_entry";
  ]
  @ body_directives @ [ Ret ]
  @ List.concat_map (compile_defn defns) defns
