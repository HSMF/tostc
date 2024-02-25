open Lib
open! Printf

let pop_extension path =
  let rec until_last = function
    | [] | [ _ ] -> []
    | x :: xs -> x :: until_last xs
  in
  match String.split_on_char '.' path with
  | [ v ] -> v
  | l -> l |> until_last |> String.concat "."


let () =
  let files = ref [] in
  Arg.parse [] (fun file -> files := file :: !files) "tostc";
  let files = List.rev !files in
  (* eprintf "debug: processing %s\n" @@ sl id ", " files; *)
  let output_path p = sp "output/%s" p in
  List.iter
    (fun filename ->
      let open Lib in
      let read_input =
        { open_file = (fun () -> open_in filename); close_file = (fun f -> close_in f) }
      in
      let no_ext = pop_extension filename in
      let write_parsed =
        { open_file = (fun () -> stdout); close_file = (fun _ -> flush stdout) }
      in
      let write_llvm =
        { open_file =
            (fun () ->
              let f' = output_path @@ no_ext ^ ".ll" in
              open_out f')
        ; close_file = (fun f -> close_out f)
        }
      in
      Lib.process_file ~read_input ~write_parsed ~write_llvm filename)
    files;
  ()
