open Ast
open Util
open Types

type elt =
  | L of Llvm.lbl
  | I of Llvm.uid * Llvm.insn
  | T of Llvm.term
  | G of Llvm.gid * Llvm.gdecl
  | E of Llvm.uid * Llvm.insn

type stream = elt list

let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x
let f a b c = c :: b :: a

let cfg_of_stream (s : stream) : Llvm.cfg * (Llvm.gid * Llvm.gdecl) list =
  let f
    ((blocks : (string * Llvm.block) list), globals, terminator, entry_insns, insns)
    (elt : elt)
    =
    match elt with
    | G (i, v) -> blocks, (i, v) :: globals, terminator, entry_insns, insns
    | E (i, ins) -> blocks, globals, terminator, (i, ins) :: entry_insns, insns
    | I (i, ins) -> blocks, globals, terminator, entry_insns, (i, ins) :: insns
    | T term -> blocks, globals, Some ("term", term), entry_insns, insns
    | L lbl -> begin
      match terminator with
      | None -> invalid_arg @@ sp "block with label `%s` has no terminator" lbl
      | Some term -> (lbl, { insns; term }) :: blocks, globals, None, entry_insns, []
    end
  in
  let labeled, globals, terminator, entry_insns, insns =
    List.fold_left f ([], [], None, [], []) s
  in
  match terminator with
  | None -> invalid_arg "no terminator found"
  | Some term -> { entry = { insns = entry_insns @ insns; term }; labeled }, globals


module Ctxt = struct
  module SMap = Map.Make (String)
  include SMap

  (** A mapping from Tost identifiers to LLVM operand + the associated type *)
  type t = (Llvm.typ * Llvm.operand) SMap.t

  let empty : t = SMap.empty
  let add ctxt key (bnd : Llvm.typ * Llvm.operand) = SMap.add key bnd ctxt

  let find key ctxt : Llvm.typ * Llvm.operand =
    match find_opt key ctxt with
    | Some x -> x
    | None -> failwith @@ sp "`%s` is not bound in the context" key


  let of_seq : _ -> t = SMap.of_seq
end

(** generate a unique identifier *)
let sym : string -> string =
  let c = ref 0 in
  fun s ->
    incr c;
    sp ".%s.%d" s !c


let item_types = function
  | Func { elt = name, happiness, args, rety, _; _ } ->
    [ ( name
      , Function (List.map (snd >>> typ_of_ast_typ) args, happiness, typ_of_ast_typ rety)
      )
    ]


let deref =
  let open Llvm in
  function
  | LPtr t -> t
  | x -> invalid_arg @@ sp "%s is not a pointer" (Llvmlib.typ x)


let rec compile_type (t : Types.typ) : Llvm.typ =
  let open Llvm in
  match t with
  | Int -> LInt
  | Uint -> LInt
  | Char -> LByte
  | Tuple [] -> LVoid
  | Function (args, _, ret) -> LFun (List.map compile_type args, compile_type ret)
  | String (* TODO: how to represent strings? *) | Var _ | Tuple _ ->
    failwith @@ sp "TODO: %s" (string_of_typ t)


let rec builtins () =
  let builtin_bop f (c : Ctxt.t) (exprs : expr node list)
    : Llvm.typ * Llvm.operand * stream
    =
    match exprs with
    | [ a; b ] -> f c (compile_expr c a) (compile_expr c b)
    | _ -> invalid_arg "tried to call a binary operation with not 2 arguments"
  in
  let open Llvm in
  [ ( ".__builtin.add_int"
    , builtin_bop (fun _ (_, op1, s1) (_, op2, s2) ->
        let out = sym "add" in
        LInt, Id out, s1 >@ s2 >:: I (out, Llvm.Binop (Add, LInt, op1, op2))) )
  ; ( ".__builtin.sub_int"
    , builtin_bop (fun _ (_, op1, s1) (_, op2, s2) ->
        let out = sym "sub" in
        LInt, Id out, s1 >@ s2 >:: I (out, Llvm.Binop (Sub, LInt, op1, op2))) )
  ; ( ".__builtin.mul_int"
    , builtin_bop (fun _ (_, op1, s1) (_, op2, s2) ->
        let out = sym "mul" in
        LInt, Id out, s1 >@ s2 >:: I (out, Llvm.Binop (Mul, LInt, op1, op2))) )
  ; ( ".__builtin.div_int"
    , builtin_bop (fun _ (_, op1, s1) (_, op2, s2) ->
        let out = sym "div" in
        LInt, Id out, s1 >@ s2 >:: I (out, Llvm.Binop (SDiv, LInt, op1, op2))) )
  ; ( ".__builtin.div_uint"
    , builtin_bop (fun _ (_, op1, s1) (_, op2, s2) ->
        let out = sym "div" in
        LInt, Id out, s1 >@ s2 >:: I (out, Llvm.Binop (UDiv, LInt, op1, op2))) )
  ]


and is_builtin s = List.mem s (List.map fst (builtins ()))

and compile_expr (c : Ctxt.t) (e : expr node) : Llvm.typ * Llvm.operand * stream =
  match e.elt with
  | EVar v ->
    let ty, op = Ctxt.find v c in
    let ty' = deref ty in
    let res = sym @@ sp "load_%s" v in
    ty', Id res, [] >:: I (res, Load (ty', op))
  | ECall ({ elt = EVar fn; _ }, _happiness, args) -> begin
    match List.assoc_opt fn (builtins ()) with
    | Some cmp_builtin -> cmp_builtin c args
    | None -> failwith "TODO: call"
  end
  | EInt i -> Llvm.LInt, Llvm.ConstInt i, []
  | ETuple _ | EIf (_, _, _) | ECall _ | EString _ | EFormatString _ ->
    failwith "TODO: compile_expr"
  | EUop (_, _) | EBop (_, _, _) ->
    invalid_arg "unary and binary operations should be eliminated before calling compile"


and compile_statement (c : Ctxt.t) (s : stmt node) : Ctxt.t * stream =
  match s.elt with
  | SReturn { elt = ETuple []; _ } -> c, [ T RetVoid ]
  | SReturn exp ->
    let ty, op, s = compile_expr c exp in
    c, s >:: T (Ret (ty, op))
  | SGive _ | SIf (_, _, _) | SLet (_, _) -> failwith "TODO: compile_statement"


let compile_function (c : Ctxt.t) ((name, _happiness, args, retty, body) : func)
  : string * Llvm.fdecl * (Llvm.gid * Llvm.gdecl) list
  =
  let c, init =
    let open Llvm in
    List.fold_left
      (fun (c, init) (arg, typ) ->
        let arg_loc = sym arg in
        let ty = compile_type (typ_of_ast_typ typ) in
        ( Ctxt.add c arg (LPtr ty, Llvm.Id arg_loc)
        , init >:: E (arg_loc, Alloca ty) >:: E ("", Store (ty, Id arg, Id arg_loc)) ))
      (c, [])
      args
  in
  let _, body =
    List.fold_left
      (fun (ctxt, body) stmt ->
        let ctxt, stmt_stream = compile_statement ctxt stmt in
        ctxt, body >@ stmt_stream)
      (c, [])
      body
  in
  let body = init >@ body in
  let cfg, globals = cfg_of_stream body in
  ( name
  , { typ =
        ( List.map (snd >>> Types.typ_of_ast_typ >>> compile_type) args
        , compile_type (Types.typ_of_ast_typ retty) )
    ; param = List.map fst args
    ; body = cfg
    }
  , globals )


let compile_prog (prog : item list) : Llvm.prog =
  let top_level_symbols = List.concat_map item_types prog in
  let functions =
    List.filter_map
      (function
       | Func { elt; _ } -> Some elt
       | _ -> None)
      prog
  in
  let c =
    top_level_symbols
    |> List.to_seq
    |> Seq.map (fun (name, typ) -> name, (compile_type typ, Llvm.Gid name))
    |> Ctxt.of_seq
  in
  let func_decls, global_decls =
    List.fold_left
      (fun (fd, glob) func ->
        let name, fd', glob' = compile_function c func in
        (name, fd') :: fd, glob @ glob')
      ([], [])
      functions
  in
  (* shouldn't really matter *)
  let func_decls = List.rev func_decls in
  { func_decls; type_decls = []; extern_decls = []; global_decls }
