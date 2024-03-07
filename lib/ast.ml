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
type item =
  | Func of func node
  | Recipe of id * recipe_item list
  | Bake of id * id * bake_item list
  (* both target and recipe should probably be paths *)

and func = id * happiness * arg list * ty * block
and typedef = id * ty
and happiness = bool
and arg = id * ty

and ty =
  | TVar of type_id
  | TTuple of ty list

(** a block of code *)
and block = stmt node list

and stmt =
  | SReturn of expr node
  | SGive of expr node
  | SIf of expr node * block * block option
  | SLet of pattern * expr node

and pattern = PId of id

and expr =
  | EVar of id
  | EAccess of expr node * id
  | ETuple of expr node list
  | EBop of bop * expr node * expr node
  | EUop of uop * expr node
  | EInt of int64
  | EIf of expr node * block * block option
  | EString of string
  | EFormatString of segment list
  | ECall of expr node * happiness * expr node list

and segment =
  | Fragment of string
  | SegExpr of expr node

and bop =
  | BopAdd
  | BopSub
  | BopMul
  | BopDiv
  | BopExp
  | BopShr
  | BopShl
  | BopLogAnd
  | BopLogOr
  | BopLogXor
  | BopBitAnd
  | BopBitOr
  | BopBitXor
  | BopLess
  | BopLessEqual
  | BopGreater
  | BopGreaterEqual
  | BopEqual
  | BopNotEqual

and uop =
  | UopPos
  | UopNegative
  | UopNeg
  | UopFlip

and recipe_item =
  | ToastDef of id
  | ToasterStub of (id * happiness * arg list * ty) node
  | Toaster of func node

and bake_item =
  | BToaster of func node
  | BToast of typedef node
