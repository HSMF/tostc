open Ast
open Util

let item_types =
  let open Types in
  function
  | Func { elt = name, happiness, args, rety, _; _ } ->
    [ ( name
      , Function (List.map (snd >>> typ_of_ast_typ) args, happiness, typ_of_ast_typ rety)
      )
    ]


let compile_prog (prog : item list) : Llvm.prog =
  let top_level_symbols = List.concat_map item_types prog in
  failwith "TODO: compile_prog"
