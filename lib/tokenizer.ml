open Tokens
open Util
open Sedlexing
open! Printf

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let newline = [%sedlex.regexp? '\n' | '\r', '\n' | '\r']
let ident_char = [%sedlex.regexp? alphabetic | '_']
let spaces = [%sedlex.regexp? Sub (white_space, Chars "\n\r")]

let reserved =
  [ "(", OpenParen
  ; ")", CloseParen
  ; "{", OpenBrace
  ; "}", CloseBrace
  ; "toaster", Toaster
  ; ":>", Happy
  ; ":<", Sad
  ; ":", Colon
  ; "->", Arrow
  ; ";", Semicolon
  ; "+", Plus
  ; "-", Minus
  ; "*", Times
  ; "/", Divide
  ; ",", Comma
  ]
  |> List.to_seq
  |> Hashtbl.of_seq


let create_token lexbuf =
  let str = Utf8.lexeme lexbuf in
  Hashtbl.find_opt reserved str |> Option.fold ~none:(Ident str) ~some:id


let create_int lexbuf =
  let str = Utf8.lexeme lexbuf in
  Int (Int64.of_string str)


(* TODO: implement tokenizer *)
let rec token buf =
  let produce tok = Seq.Cons (tok, fun () -> token buf) in
  match%sedlex buf with
  | Plus spaces -> token buf
  | newline ->
    new_line buf;
    token buf
  | "//" -> line_comment buf
  | "/*" -> block_comment 1 buf
  | ":>" | ":<" | "->" | Chars "(){}+-*/;:," -> produce @@ create_token buf
  | Opt (Chars "+-"), number -> produce @@ create_int buf
  | ident_char, Star (ident_char | digit) -> produce @@ create_token buf
  | eof -> Seq.Nil
  | any -> failwith @@ sp "Unexpected character %s" (Utf8.lexeme buf)
  | _ -> failwith "unreachable state"


and line_comment buf =
  match%sedlex buf with
  | newline ->
    new_line buf;
    token buf
  | any -> line_comment buf
  | _ -> failwith "unreachable state"


and block_comment n buf =
  match%sedlex buf with
  | "/*" -> block_comment (n + 1) buf
  | "*/" -> if n = 1 then token buf else block_comment (n - 1) buf
  | newline ->
    new_line buf;
    block_comment n buf
  | any -> block_comment n buf
  | _ -> failwith "unreachable state"


let lex buf : token Seq.t =
  Seq.inspect (fun x -> printf "%s " @@ string_of_token x) @@ fun () -> token buf
