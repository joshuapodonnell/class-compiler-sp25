(* This file is not used by the compiler, it is a demonstration of 
   a hand-rolled compiler.
   
   You can load it with "dune utop" followed by "open Lib.Handparser;;"
*)

type s_exp = Num of int | Sym of string | Lst of s_exp list
type token = NUM of int | SYM of string | LPAREN | RPAREN

exception ParseError

(* bad tokenizer, do not use something like this *)
let token_of_string (s : string) =
  match s with
  | "(" -> LPAREN
  | ")" -> RPAREN
  | _ -> ( try NUM (int_of_string s) with _ -> SYM s)

let tokenize (s : string) =
  s |> String.split_on_char ' ' |> List.map token_of_string

(* end bad tokenizer *)

let rec parse_s_exp (toks : token list) : s_exp * token list = failwith "TODO"
