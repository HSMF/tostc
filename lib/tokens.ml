(** type definitions for the tokens. included by lexer.mll and grammar.ast *)

open Util

type token =
  | OpenParen
  | CloseParen
  | OpenBrace
  | CloseBrace
  | Toaster
  | Happy
  | Sad
  | If
  | Else
  | Let
  | Colon
  | Arrow
  | Semicolon
  (* unary operators *)
  | LogNot
  | BinNot
  | Plus
  | Minus
  (* binary operators *)
  | Times
  | Divide
  | Exponent
  | ShiftRight
  | ShiftLeft
  | LogAnd
  | LogOr
  | LogXor
  | BitAnd
  | BitOr
  | BitXor
  | Less
  | LessEqual
  | GreaterEqual
  | Greater
  | Equals
  | NotEquals
  | Assign
  | Dot
  | Comma
  | Return
  | Give
  | String of string
  | StringFragment of string
  | BeginString
  | EndString
  | Ident of string
  | Int of int64

(* TODO: add tokens *)

let string_of_token = function
  | OpenParen -> "OpenParen"
  | CloseParen -> "CloseParen"
  | Ident x -> sp "Ident (%s)" x
  | Int x -> sp "Int (%Ld)" x
  | String s -> sp "String (\"%s\")" (String.escaped s)
  | StringFragment s -> sp "StringFragment (\"%s\")" (String.escaped s)
  | BeginString -> "BeginString"
  | EndString -> "EndString"
  | Toaster -> "Toaster"
  | Happy -> "Happy"
  | Sad -> "Sad"
  | If -> "If"
  | Else -> "Else"
    | Let -> "Let"
  | Colon -> "Colon"
  | OpenBrace -> "OpenBrace"
  | CloseBrace -> "CloseBrace"
  | Arrow -> "Arrow"
  | Semicolon -> "Semicolon"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Divide -> "Divide"
  | Comma -> "Comma"
  | Assign -> "Assign"
  | Equals -> "Equals"
  | NotEquals -> "NotEquals"
  | Dot -> "Dot"
  | Return -> "Return"
  | Give -> "Give"
  | LogNot -> "LogNot"
  | BinNot -> "BinNot"
  | Exponent -> "Exponent"
  | ShiftRight -> "ShiftRight"
  | ShiftLeft -> "ShiftLeft"
  | LogAnd -> "LogAnd"
  | LogOr -> "LogOr"
  | LogXor -> "LogXor"
  | BitAnd -> "BitAnd"
  | BitOr -> "BitOr"
  | BitXor -> "BitXor"
  | Less -> "Less"
  | LessEqual -> "LessEqual"
  | GreaterEqual -> "GreaterEqual"
  | Greater -> "Greater"
