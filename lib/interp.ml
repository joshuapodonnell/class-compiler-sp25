open S_exp
open Shared.Error
open Symtab
open Util

type value = Number of int | Boolean of bool | Pair of (value * value)

let int_of_value (v : value) : int =
  match v with Number n -> n | _ -> failwith "not a num!"

let rec string_of_value (v : value) : string =
  match v with
  | Number n -> string_of_int n
  | Boolean b -> string_of_bool b
  | Pair (v1, v2) ->
      Printf.sprintf "(pair %s %s)" (string_of_value v1) (string_of_value v2)

let input_channel = ref stdin
let output_channel = ref stdout

let rec interp_exp (defns : defn list) (env : value symtab) (exp : s_exp) :
    value =
  (* interp2 is a helper to force the evaluation of 2 exprs in order *)
  let interp2 defns env e1 e2 =
    let v1 = interp_exp defns env e1 in
    let v2 = interp_exp defns env e2 in
    (v1, v2)
  in
  match exp with
  | Num n -> Number n
  | Sym "false" -> Boolean false
  | Sym "true" -> Boolean true
  | Sym var -> Symtab.find var env
  | Lst (Sym f :: args) when is_defn defns f ->
      let defn = get_defn defns f in
      if List.length args = List.length defn.args then
        let vals = List.map (interp_exp defns env) args in
        let pairs = List.combine defn.args vals in
        let fenv =
          List.fold_left
            (fun env (k, v) -> Symtab.add k v env)
            Symtab.empty pairs
        in
        interp_exp defns fenv defn.body
      else failwith "wrong number of args"
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; e ] ]; e_body ] ->
      let e_value = interp_exp defns env e in
      interp_exp defns (Symtab.add var e_value env) e_body
  | Lst [ Sym "add1"; l ] -> Number (int_of_value (interp_exp defns env l) + 1)
  | Lst [ Sym "sub1"; l ] -> Number (int_of_value (interp_exp defns env l) - 1)
  | Lst [ Sym "not"; l ] ->
      if interp_exp defns env l = Boolean false then Boolean true
      else Boolean false
  | Lst [ Sym "zero?"; l ] ->
      if interp_exp defns env l = Number 0 then Boolean true else Boolean false
  | Lst [ Sym "num?"; l ] -> (
      match interp_exp defns env l with
      | Number _ -> Boolean true
      | _ -> Boolean false)
  | Lst [ Sym "if"; e_cond; e_then; e_else ] ->
      let v_cond = interp_exp defns env e_cond in
      if v_cond = Boolean false then interp_exp defns env e_else
      else interp_exp defns env e_then
  | Lst [ Sym "+"; e1; e2 ] ->
      let v1, v2 = interp2 defns env e1 e2 in
      Number (int_of_value v1 + int_of_value v2)
  | Lst [ Sym "-"; e1; e2 ] ->
      let v1, v2 = interp2 defns env e1 e2 in
      Number (int_of_value v1 - int_of_value v2)
  | Lst [ Sym "="; e1; e2 ] -> (
      match interp2 defns env e1 e2 with
      | Number n1, Number n2 -> Boolean (n1 = n2)
      | _ -> raise (Stuck exp))
  | Lst [ Sym "<"; e1; e2 ] -> (
      match interp2 defns env e1 e2 with
      | Number n1, Number n2 -> Boolean (n1 < n2)
      | _ -> raise (Stuck exp))
  | Lst [ Sym "pair"; e1; e2 ] -> Pair (interp2 defns env e1 e2)
  | Lst [ Sym "left"; e ] -> (
      match interp_exp defns env e with
      | Pair (v, _) -> v
      | _ -> failwith "not a pair")
  | Lst [ Sym "right"; e ] -> (
      match interp_exp defns env e with
      | Pair (_, v) -> v
      | _ -> failwith "not a pair")
  | Lst [ Sym "read-num" ] -> Number (input_line !input_channel |> int_of_string)
  | Lst [ Sym "print"; e ] ->
      let v = interp_exp defns env e in
      output_string !output_channel (string_of_value v);
      Boolean true
  | Lst [ Sym "newline" ] ->
      output_string !output_channel "\n";
      Boolean true
  | Lst (Sym "do" :: exprs) ->
      List.map (interp_exp defns env) exprs |> ignore;
      Boolean true
  | _ -> raise (Stuck exp)

let interp (program : s_exp list) : unit =
  let defns, body = defns_and_body program in
  interp_exp defns Symtab.empty body |> ignore

let interp_io (input : string) (program : s_exp list) =
  let input_pipe_ex, input_pipe_en = Unix.pipe () in
  let output_pipe_ex, output_pipe_en = Unix.pipe () in
  input_channel := Unix.in_channel_of_descr input_pipe_ex;
  set_binary_mode_in !input_channel false;
  output_channel := Unix.out_channel_of_descr output_pipe_en;
  set_binary_mode_out !output_channel false;
  let write_input_channel = Unix.out_channel_of_descr input_pipe_en in
  set_binary_mode_out write_input_channel false;
  let read_output_channel = Unix.in_channel_of_descr output_pipe_ex in
  set_binary_mode_in read_output_channel false;
  output_string write_input_channel input;
  close_out write_input_channel;
  interp program;
  close_out !output_channel;
  let r = In_channel.input_all read_output_channel in
  input_channel := stdin;
  output_channel := stdout;
  r
