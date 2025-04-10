open Ast

let rec fold : expr -> expr = function e -> e

let fold_program (prog : program) =
  {
    body = fold prog.body;
    defns =
      List.map
        (fun { name; args; body } -> { name; args; body = fold body })
        prog.defns;
  }
