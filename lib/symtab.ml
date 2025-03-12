open String

module Symtab0 = Map.Make (struct
  type t = string

  let compare = compare
end)

module Symtab = struct
  include Symtab0

  let of_list l = l |> List.to_seq |> of_seq
end

type 'a symtab = 'a Symtab.t

(* include Symtab *)
