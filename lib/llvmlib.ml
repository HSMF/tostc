open Llvm
open Util

let rec indent depth s = if depth <= 0 then s else "  " ^ indent (depth - 1) s
let uid (u : uid) = "%" ^ u
let gid (u : gid) = "@" ^ u
let tid (u : tid) = u
let lbl (u : lbl) = u

let rec typ = function
  | LVoid -> "void"
  | LBool -> "i1"
  | LInt -> "i64"
  | LByte -> "i8"
  | LFun (args, ret) -> sp "%s (%s)" (sl typ ", " args) (typ ret)
  | LPtr _ -> "ptr"
  | LStruct ts -> sp "%s" (sl typ ", " ts)
  | LArray (n, t) -> sp "[%d x %s]" n (typ t)
  | LNamed t -> tid t


let operand = function
  | Null -> "null"
  | Id i -> uid i
  | Gid g -> gid g
  | ConstInt i -> sp "%Ld" i


let bop = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Udiv -> "udiv"
  | SDiv -> "sdiv"
  | URem -> "urem"
  | SRem -> "srem"
  | Shl -> "shl"
  | LShr -> "lhsr"
  | AShr -> "ashr"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"


let cnd = function
  | Eq -> "eq"
  | Ne -> "ne"
  | UGt -> "ugt"
  | UGe -> "uge"
  | ULt -> "ult"
  | Ule -> "ule"
  | SGt -> "sgt"
  | SGe -> "sge"
  | SLt -> "slt"
  | SLe -> "sle"


let insn (i, ins) =
  let s_ins =
    match ins with
    | Binop (which, ty, op1, op2) ->
      sp "%s %s %s, %s" (bop which) (typ ty) (operand op1) (operand op2)
    | ICmp (cnd', ty, op1, op2) ->
      sp "icmp %s %s %s, %s" (cnd cnd') (typ ty) (operand op1) (operand op2)
    | Alloca ty -> sp "alloca %s" (typ ty)
    | Load (ty, ptr) -> sp "load %s, ptr %s" (typ ty) (operand ptr)
    | Store (ty, src, dest) ->
      sp "store %s %s, ptr %s" (typ ty) (operand src) (operand dest)
    | Call (ty, fn, args) ->
      sp
        "call %s %s(%s)"
        (typ ty)
        (operand fn)
        (sl (fun (ty, arg) -> sp "%s %s" (typ ty) (operand arg)) ", " args)
    | Gep (ty, a, []) -> sp "getelementptr %s, ptr %s" (typ ty) (operand a)
    | Gep (ty, a, indices) ->
      sp
        "getelementptr %s, ptr %s, %s"
        (typ ty)
        (operand a)
        (sl (operand >>> sp "i64 %s") ", " indices)
    | Bitcast (_, _, _) -> ""
  in
  match ins with
  | Store _ | Call (LVoid, _, _) -> s_ins
  | _ -> sp "%s = %s" (uid i) s_ins


let term : term -> string = function
  | RetVoid -> "ret void"
  | Ret (t, op) -> sp "ret %s %s" (typ t) (operand op)
  | Br (cnd, yes, no) ->
    sp "br i1 %s, label %s, label %s" (operand cnd) (lbl yes) (lbl no)
  | BrUncond l -> sp "br label %s" (lbl l)
  | Switch (intty, value, defaultdest, cases) ->
    sp
      "switch %s %s, label %s, [%s]"
      (typ intty)
      (operand value)
      (lbl defaultdest)
      (sl
         (fun (ty, value, label) ->
           sp "%s %s, label %s" (typ ty) (operand value) (lbl label))
         " "
         cases)


let block { insns; term = _, term' } =
  sp "%s\n%s" (sl (insn >>> indent 2) "\n" insns) (term term')


let cfg { entry; labeled } =
  sp
    "  %s\n%s"
    (block entry)
    (sl (fun (label, b) -> sp "  %s:\n%s" label (block b)) "\n" labeled)


let fdecl (name, { typ = args, ret; param; body }) =
  sp
    "define %s %s(%s) {\n%s\n}"
    (typ ret)
    (gid name)
    (sl (fun (ty, par) -> sp "%s %s" (typ ty) (uid par)) ", " (List.zip args param))
    (cfg body)


let string_of_prog { type_decls; func_decls; global_decls; extern_decls } =
  sl fdecl "\n\n" func_decls ^ "\n\n\n"
