open Lib
open Printf

let process_file filename =
  let f = open_in filename in
  printf "processing %s\n" filename;
  let parsed = f |> lex_channel |> parse_tokens in
  close_in f;
  printf "received %s\n" parsed


let () =
  let files = ref [] in
  Arg.parse [] (fun file -> files := file :: !files) "tostc";
  let files = List.rev !files in
  (* eprintf "debug: processing %s\n" @@ sl id ", " files; *)
  List.iter process_file files;
  let s = "((((((hello))))))" in
  printf "parsed %s -> got %s" s (s |> lex_string |> parse_tokens);
  ()
