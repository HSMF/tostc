open Util

(** local identifier *)
type uid = string

(** global identifier *)
type gid = string

(** named types *)
type tid = string

(** labels *)
type lbl = string

type typ =
  | LVoid
  | LBool
  | LInt
  | LByte
  | LFun of typ list * typ
  | LPtr of typ
  | LStruct of typ list
  | LArray of int * typ
  | LNamed of tid

let rec string_of_typ = function
  | LVoid -> "void"
  | LBool -> "i1"
  | LInt -> "i64"
  | LByte -> "i8"
  | LFun (args, ret) -> sp "%s (%s)" (sl string_of_typ ", " args) (string_of_typ ret)
  | LPtr _ -> "ptr"
  | LStruct ts -> sp "%s" (sl string_of_typ ", " ts)
  | LArray (n, t) -> sp "[%d x %s]" n (string_of_typ t)
  | LNamed t -> t


type operand =
  | Null
  | Id of uid
  | Gid of gid
  | ConstInt of int64

let string_of_operand = function
  | Null -> "null"
  | Id i -> "%" ^ i
  | Gid g -> "@" ^ g
  | ConstInt i -> sp "%Ld" i

type bop
type cnd
type insn
type term
type block
type cfg
type prog
