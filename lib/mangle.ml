open Util

type kind =
  | Recipe
  | Module
  | Struct
  | Ident

let prefix_of_kind = function
  | Recipe -> "r"
  | Module -> "m"
  | Struct -> "s"
  | Ident -> "i"


(** mangles a identifier along the following rules:
    - each part in the path is represented as [<prefix><length><name>]
    - prefix is 'r' for recipe, 'm' for module, 's' for struct, 'i' for identifier
    - length is the length of the name, in decimal ascii
    - name is the name of the identifier. Since identifiers may not start with a
      digit in tost, this is unambiguous *)
let mangle (p : (kind * string) list) =
  List.map (fun (k, name) -> sp "%s%d%s" (prefix_of_kind k) (String.length name) name) p
  |> String.concat ""
