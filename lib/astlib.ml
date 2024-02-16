open Ast
open Util

let elt x = x.elt
let rec indent level = if level > 0 then "  " ^ indent (level - 1) else ""
let indented level s = indent level ^ s

let rec string_of_program (p : item list) =
  sl
    (function
      | Func f -> string_of_func f.elt)
    "\n\n"
    p


and string_of_func ?(indentation = 0) ((name, happy, args, retty, body) : func) =
  sp
    "toaster %s %s %s %s %s"
    name
    (if happy then ":>" else ":<")
    (sl string_of_arg ", " args)
    (match retty with
     | TTuple [] -> ""
     | _ -> sp " -> %s" (string_of_ty retty))
    (string_of_block ~indentation body)


and string_of_arg ((name, typ) : arg) = sp "%s: %s" name (string_of_ty typ)

and string_of_ty (t : ty) =
  match t with
  | TVar s -> s
  | TTuple vs -> sp "(%s)" @@ sl string_of_ty ", " vs


and string_of_block ?(indentation = 0) (b : block) =
  sp
    "{\n%s\n%s}"
    (sl (elt >>> string_of_stmt ~indentation:(indentation + 1)) "\n" b)
    (indent indentation)


and string_of_stmt ?(indentation = 0) (s : stmt) =
  begin
    match s with
    | SReturn { elt = ETuple []; _ } -> "return;"
    | SReturn exp -> sp "return %s;" (string_of_expr ~indentation exp.elt)
    | SDecl (pat, exp) ->
      sp "let %s = %s;" (string_of_pattern pat.elt) (string_of_expr exp.elt)
  end
  |> indented indentation


and string_of_pattern ?(indentation = 0) =
  ignore indentation;
  function
  | PId i -> i


and string_of_expr ?(indentation = 0) (e : expr) =
  let se = elt >>> string_of_expr ~indentation in
  match e with
  | EVar v -> v
  | ETuple els -> sp "(%s)" @@ sl se ", " els
  | EBop (op, l, r) -> sp "(%s %s %s)" (se l) (string_of_bop op) (se r)
  | EInt i -> Int64.to_string i


and string_of_bop (b : bop) =
  match b with
  | BopAdd -> "+"
  | BopSub -> "-"
  | BopMul -> "*"
  | BopDiv -> "/"
