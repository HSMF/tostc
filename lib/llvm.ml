open Util

(** local identifier *)
type uid = string

(** global identifier *)
type gid = string

(** named types *)
type tid = string

(** labels *)
type lbl = string

(** llvm types *)
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

(** operands. everything that can be used as an argument to something *)
type operand =
  | Null
  | Id of uid
  | Gid of gid
  | ConstInt of int64

(** type of binary operator *)
type bop =
  | Add
  | Sub
  | Mul
  | Udiv
  | SDiv
  | URem
  | SRem
  | Shl
  | LShr
  | AShr
  | And
  | Or
  | Xor

(** type of comparison operator *)
type cnd =
  | Eq
  | Ne
  | UGt
  | UGe
  | ULt
  | Ule
  | SGt
  | SGe
  | SLt
  | SLe

(** instruction *)
type insn =
  | Binop of bop * typ * operand * operand
  | ICmp of cnd * typ * operand * operand
  | Alloca of typ
  | Load of typ * operand
  | Store of typ * operand * operand
  | Call of typ * operand * (typ * operand) list
  | Gep of typ * operand * operand list
  | Bitcast of typ * operand * typ

(** terminator instruction *)
type term =
  | RetVoid
  | Ret of typ * operand
  | Br of operand * lbl * lbl
  | BrUncond of lbl
  | Switch of typ * operand * lbl * (typ * operand * lbl) list

(** block as defined by llvm *)
type block =
  { insns : (uid * insn) list
  ; term : uid * term
  }

(** control flow graph *)
type cfg =
  { entry : block
  ; labeled : (lbl * block) list
  }

(** function declaration *)
type fdecl =
  { typ : typ list * typ
  ; param : uid list
  ; body : cfg
  }

(** global initializer *)
type ginit =
  | GNull
  | GGid of gid
  | GConstInt of int64
  | GString of string
  | GArray of (typ * ginit) list
  | GStruct of (typ * ginit) list
  | GBitcast of typ * ginit * typ

(** global declaration *)
type gdecl = typ * ginit

(** program *)
type prog =
  { type_decls : (tid * typ) list
  ; func_decls : (gid * fdecl) list
  ; global_decls : (gid * gdecl) list
  ; extern_decls : (gid * typ) list
  }
