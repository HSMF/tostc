open Tokens
open Util
open Sedlexing

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let newline = [%sedlex.regexp? '\n' | '\r', '\n' | '\r']
let ident_char = [%sedlex.regexp? alphabetic | '_']
let spaces = [%sedlex.regexp? Intersect (Compl (Chars "\n\r"), white_space)]
let reserved = [ "(", OpenParen; ")", CloseParen ] |> List.to_seq |> Hashtbl.of_seq

let create_token lexbuf =
  let str = Utf8.lexeme lexbuf in
  Hashtbl.find_opt reserved str |> Option.fold ~none:(Ident str) ~some:id


(* TODO: implement tokenizer *)
let rec token buf =
  match%sedlex buf with
  | Plus spaces -> token buf
  | newline ->
    new_line buf;
    token buf
  | Chars "()" -> Seq.Cons (create_token buf, fun () -> token buf)
  | ident_char, Star (ident_char | digit) ->
    Seq.Cons (create_token buf, fun () -> token buf)
  | eof -> Seq.Nil
  | _ -> failwith "Unexpected character"


let lex buf : token Seq.t = fun () -> token buf
