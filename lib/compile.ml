open S_exp
open Asm

let rec compile_exp (exp: s_exp): directive list = 
  match exp with 
  Num n -> [ Mov (Reg Rax, Imm n); ] 
  | Lst [Sym "add1"; l] -> 
    let p = compile_exp l in
    p @ [ Add (Reg Rax, Imm 1)]
  | Lst [Sym "sub1"; l] -> 
    let p = compile_exp l in
    p @ [ Sub (Reg Rax, Imm 1)]
  | _ -> failwith "i dont know"

let compile_fun (exp: s_exp): directive list =
  let directives = compile_exp exp in
  [ Global "entry"; Label "entry" ] @ directives @ [ Ret ] 

let compile (program: string) : string =
  let sexp = parse program in
  compile_fun sexp |> List.map string_of_directive |> String.concat "\n"

let compile_to_file (program: string): unit = 
  let file = open_out "program.s" in
  output_string file (compile program);
  close_out file

let compile_and_run (program: string): string =
  compile_to_file program;
  let _ = Unix.system "nasm program.s -f macho64" in
  let _ = Unix.system "clang -arch x86_64 program.o runtime.c" in
  let input = Unix.open_process_in "./a.out" in
  let response = input_line input in
  close_in input; response

open Interp

let difftest (programs: string list): bool = 
  let result_pairs = List.map (fun p -> (compile_and_run p, interp p)) programs in
  List.for_all (fun (r1, r2) -> r1 = r2) result_pairs

let test () = difftest [
  "42";
  "(add1 (sub1 42))";
  "(add1 (add1 (sub1 1)))";
]