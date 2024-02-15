include Util
open Tokens

let lex_string s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  Tokenizer.lex lexbuf


let lex_channel ch =
  let lexbuf = Sedlexing.Utf8.from_channel ch in
  Tokenizer.lex lexbuf


let parse_tokens (toks : Tokens.token Seq.t) =
  let toks = List.of_seq toks in
  Printf.printf "parsing with [%s]\n" @@ sl string_of_token ", " toks;
  Grammar.parse toks
