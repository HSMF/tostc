(** combination of an item with its location in the source file.
    Used for error reporting *)
type 'a node =
  { elt : 'a
  ; location : Span.span
  }

let no_loc elt = { elt; location = Span.none }

(** identifies a [value] in a given context *)
type id = string

(** identifies a [type] in a given context *)
type type_id = string

(** a function, fields are: function name, arguments, return type, body *)
type item = Func of func node

and func = id * happiness * arg list * ty * block
and happiness = bool
and arg = id * ty

and ty =
  | TVar of type_id
  | TTuple of ty list

and block = stmt node list

and stmt =
  | SReturn of expr node
  | SDecl of pattern node * expr node

and pattern = PId of id

and expr =
  | EVar of id
  | ETuple of expr node list
  | EBop of bop * expr node * expr node

and bop =
  | BopAdd
  | BopSub
  | BopMul
  | BopDiv
