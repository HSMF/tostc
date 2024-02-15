open Lib
open Printf

let () =
  let files = ref [] in
  Arg.parse [] (fun file -> files := file :: !files) "tostc";
  let files = List.rev !files in
  print_endline @@ sl id ", " files;
  let s = "((((((hello))))))" in
  printf "parsed %s -> got %s" s (s |> lex_string |> parse_tokens);
  ()
