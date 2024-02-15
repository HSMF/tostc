include Util
open Tokens

let lex_string s =
  let s = s |> String.to_seq |> Seq.map Uchar.of_char |> Array.of_seq in
  let lexbuf = Sedlexing.from_uchar_array s in
  Tokenizer.lex lexbuf


let parse_tokens (toks : Tokens.token Seq.t) =
  let toks = List.of_seq toks in
  Printf.printf "parsing with [%s]\n" @@ sl string_of_token ", " (toks);
  Grammar.parse (toks)
