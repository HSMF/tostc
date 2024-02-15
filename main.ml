open Lib
open Printf



let () =
  let files = ref [] in
  Arg.parse [] (fun file -> files := file :: !files) "tostc";
  let files = List.rev !files in
  (* eprintf "debug: processing %s\n" @@ sl id ", " files; *)
  List.iter Lib.process_file files;
  ()
