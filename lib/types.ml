open Util

type typ =
  | Int
  | String
  | Char
  | Var of string
  | Tuple of typ list
  | Function of typ list * bool * typ

let rec string_of_typ = function
  | Int -> "int"
  | String -> "string"
  | Char -> "char"
  | Var s -> s
  | Tuple ts -> sp "(%s)" (sl string_of_typ ", " ts)
  | Function (args, true, ret) ->
    sp "(%s) :> %s" (sl string_of_typ ", " args) (string_of_typ ret)
  | Function (args, false, ret) ->
    sp "(%s) :< %s" (sl string_of_typ ", " args) (string_of_typ ret)


let rec typ_of_ast_typ : Ast.ty -> typ = function
  | Ast.TVar "int" -> Int
  | Ast.TVar "string" -> String
  | Ast.TVar "char" -> Char
  | Ast.TVar s -> Var s
  | Ast.TTuple ts -> Tuple (List.map typ_of_ast_typ ts)
