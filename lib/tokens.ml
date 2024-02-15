(** type definitions for the tokens. included by lexer.mll and grammar.ast *)

open Util

type token =
  | OpenParen
  | CloseParen
  | Ident of string

(* TODO: add tokens *)

let string_of_token = function
  | OpenParen -> "OpenParen"
  | CloseParen -> "CloseParen"
  | Ident x -> sp "Ident (%s)" x
