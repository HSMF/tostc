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


type 'a file =
  { open_file : unit -> 'a
  ; close_file : 'a -> unit
  }

let write_string strategy s =
  let f = strategy.open_file () in
  output_string f s;
  strategy.close_file f


let process_file
  ~(read_input : in_channel file)
  ~(write_parsed : out_channel file)
  ~(write_llvm : out_channel file)
  ~(compile_file : unit -> unit)
  filename
  =
  let f = read_input.open_file () in
  printf "processing %s\n" filename;
  let parsed = f |> lex_channel |> parse_tokens in
  read_input.close_file f;
  write_string write_parsed @@ sp "received:\n%s\n" (Astlib.string_of_program parsed);
  let normalized = List.map Type_normalize.normalize_item parsed in
  write_string write_parsed
  @@ sp "normalized:\n%s\n" (Astlib.string_of_program normalized);
  let llvm = Compile.compile_prog normalized in
  write_string write_llvm @@ Llvmlib.string_of_prog llvm;
  compile_file ();
  ()
