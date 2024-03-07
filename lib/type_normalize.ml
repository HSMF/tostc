(** dispatches operators and generic function calls according to the associated recipe *)
open Ast

open Util

module RecipeCtxt = struct
  type t = unit

  open Types

  (** type of a item in a recipe. Either a type name, or a function
      [FuncDef ( name, happiness, arguments, return_type, required )]
      If a function required that means that its body is not supplied in the recipe definition
      and an implementation must be provided by every baker *)
  type item =
    | Type of id
    | FuncDef of id * happiness * typ list * typ * bool

  let empty = ()

  (** registers a recipe. *)
  let register_recipe (name : id) (items : recipe_item list) = failwith "-"

  (** registers an implementor. The recipe must already be registered.
      complains if the implementor doesnt adhere to the specification *)
  let register_implementor (typename : id) (recipe : id) (body : bake_item list) =
    failwith "-"
end

let rename_func n (_, happiness, args, ty, block) : func = n, happiness, args, ty, block

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

let rec normalize_expr (c : RecipeCtxt.t) (e : expr node) : expr node =
  let call name args = ECall (no_loc @@ EVar name, false, args) in
  let elt =
    match e.elt with
    | EVar _ | EInt _ | EString _ | EFormatString _ -> e.elt
    | EAccess (e, field) -> EAccess (normalize_expr c e, field)
    | ETuple es -> ETuple (List.map (normalize_expr c) es)
    | EBop (op, e1, e2) ->
      let t1 = type_expr e1 in
      let t2 = type_expr e2 in
      let builtin = find_builtin_bop (op, t1, t2) in
      call builtin [ normalize_expr c e1; normalize_expr c e2 ]
    | EUop (_, _) -> failwith "todo"
    | EIf (cond, yes, no) ->
      EIf
        ( normalize_expr c cond
        , List.map (normalize_stmt c) yes
        , Option.map (List.map (normalize_stmt c)) no )
    | ECall (_fn, _h, _args) -> failwith "todo"
  in
  { elt; location = e.location }


and normalize_stmt (c : RecipeCtxt.t) (s : stmt node) : stmt node =
  let elt =
    match s.elt with
    | SReturn e -> SReturn (normalize_expr c e)
    | SGive e -> SGive (normalize_expr c e)
    | SIf (cond, yes, no) ->
      SIf
        ( normalize_expr c cond
        , List.map (normalize_stmt c) yes
        , Option.map (List.map (normalize_stmt c)) no )
    | SLet (pat, e) -> SLet (pat, normalize_expr c e)
  in
  { elt; location = s.location }


and normalize_func (c : RecipeCtxt.t) ((name, h, args, rety, body) : func) =
  name, h, args, rety, List.map (normalize_stmt c) body


and normalize_item (c : RecipeCtxt.t) = function
  | Func f -> [ Func { f with elt = normalize_func c f.elt } ]
  | Recipe _ -> []
  | Bake (target, recipe, body) ->
    List.map
      (function
       | BToast _ -> failwith ""
       | BToaster fn ->
         let mangled = Mangle.mangle [ Recipe, recipe; Mangle.Ident, target ] in
         Func { fn with elt = normalize_func c fn.elt |> rename_func mangled })
      body


and normalize p =
  let ctxt =
    List.fold_left
      (fun acc item ->
        match item with
        | _ -> acc)
      RecipeCtxt.empty
      p
  in
  List.concat_map (normalize_item ctxt) p
