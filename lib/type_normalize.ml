(** dispatches operators and generic function calls according to the associated recipe *)
open Ast

open Util

(* TODO: this will have to be aware of types... *)
(* TODO: and recipes... *)

let builtin = sp ".__builtin.%s"

let builtin_bops =
  let open Types in
  [ BopAdd, Int, Int, builtin "add_int"
  ; BopSub, Int, Int, builtin "sub_int"
  ; BopMul, Int, Int, builtin "mul_int"
  ; BopLess, Int, Int, builtin "less_int"
  ; BopLessEqual, Int, Int, builtin "less_equal_int"
  ; BopGreater, Int, Int, builtin "greater_int"
  ; BopGreaterEqual, Int, Int, builtin "greater_equal_int"
  ; BopEqual, Int, Int, builtin "equal_int"
  ; BopNotEqual, Int, Int, builtin "not_equal_int"
  ]
  |> List.map (fun (a, b, c, d) -> (a, b, c), d)


let builtin_uops =
  let open Types in
  [ UopPos, Int, builtin "pos_int"
  ; UopNegative, Int, builtin "neg_int"
  ; UopNeg, Int, builtin "not_int"
  ; UopFlip, Int, builtin "bit_not_int"
  ]
  |> List.map (fun (a, b, c) -> (a, b), c)


let find_builtin_bop k = List.assoc k builtin_bops
let find_builtin_uop k = List.assoc k builtin_uops
let type_expr (e : expr node) : Types.typ = Types.Int

let rec normalize_expr (e : expr node) : expr node =
  let call name args = ECall (no_loc @@ EVar name, false, args) in
  let elt =
    match e.elt with
    | EVar _ | EInt _ | EString _ | EFormatString _ -> e.elt
    | ETuple es -> ETuple (List.map normalize_expr es)
    | EBop (op, e1, e2) ->
      let t1 = type_expr e1 in
      let t2 = type_expr e2 in
      let builtin = find_builtin_bop (op, t1, t2) in
      call builtin [ normalize_expr e1; normalize_expr e2 ]
    | EUop (_, _) -> failwith "todo"
    | EIf (cond, yes, no) ->
      EIf
        ( normalize_expr cond
        , List.map normalize_stmt yes
        , Option.map (List.map normalize_stmt) no )
    | ECall (_fn, _h, _args) -> failwith "todo"
  in
  { elt; location = e.location }


and normalize_stmt (s : stmt node) : stmt node =
  let elt =
    match s.elt with
    | SReturn e -> SReturn (normalize_expr e)
    | SGive e -> SGive (normalize_expr e)
    | SIf (cond, yes, no) ->
      SIf
        ( normalize_expr cond
        , List.map normalize_stmt yes
        , Option.map (List.map normalize_stmt) no )
    | SLet (pat, e) -> SLet (pat, normalize_expr e)
  in
  { elt; location = s.location }


and normalize_func ((name, h, args, rety, body) : func) =
  name, h, args, rety, List.map normalize_stmt body


and normalize_item = function
  | Func f -> Func { f with elt = normalize_func f.elt }