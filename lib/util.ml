open S_exp
open Shared
open Error

let gensym : string -> string =
  let counter = ref 0 in
  fun s ->
    let symbol = Printf.sprintf "%s__%d" s !counter in
    counter := !counter + 1;
    symbol

type defn = {
  name : string;
  args : string list;
  rest : string option;
  body : s_exp;
}

let sym e = match e with Sym s -> s | _ -> raise (Stuck e)

let defns_and_body (exps : s_exp list) : defn list * s_exp =
  let rec args_and_rest args = function
    | [] -> (List.rev args, None)
    | [ arg; Dots ] -> (List.rev args, Some (sym arg))
    | arg :: exps -> args_and_rest (sym arg :: args) exps
  in
  let get_defn = function
    | Lst [ Sym "define"; Lst (Sym name :: args); body ] ->
        let args, rest = args_and_rest [] args in
        { name; args; rest; body }
    | e -> raise (Stuck e)
  in
  let rec go exps defns =
    match exps with
    | [ e ] -> (List.rev defns, e)
    | d :: exps -> go exps (get_defn d :: defns)
    | _ -> raise (Stuck (Sym "empty"))
  in
  go exps []

let is_defn defns name = List.exists (fun d -> d.name = name) defns
let get_defn defns name = List.find (fun d -> d.name = name) defns
