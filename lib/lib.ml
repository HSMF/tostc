include Util
open Tokens
open Printf

let lex_string s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  Tokenizer.lex lexbuf


let lex_channel ch =
  let lexbuf = Sedlexing.Utf8.from_channel ch in
  Tokenizer.lex lexbuf


let parse_tokens (toks : Tokens.token Seq.t) =
  let open Grammar in
  let toks = List.of_seq toks in
  Printf.printf "parsing with [%s]\n" @@ sl string_of_token ", " toks;
  try parse toks with
  | Parse_error (ErrMsg msg) -> failwith msg
  | Parse_error (ErrUnexpectedToken (expected, node, remainder)) -> begin
    flush_all ();
    eprintf "expected one of %s in %s\n" (sl id ", " expected) node;
    eprintf "remaining input is %s\n" (sl string_of_token " " remainder);
    exit ~-1
  end


let process_file filename =
  let f = open_in filename in
  printf "processing %s\n" filename;
  let parsed = f |> lex_channel |> parse_tokens in
  close_in f;
  printf "received:\n%s\n" (Astlib.string_of_program parsed)
