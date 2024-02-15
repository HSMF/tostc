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
  | Colon
  | Arrow
  | Semicolon
  | Plus
  | Minus
  | Times
  | Divide
  | Comma
  | Assign
  | Equals
  | NotEquals
  | Dot
  | Return
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
