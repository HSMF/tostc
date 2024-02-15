open Tokens
open Util
open Sedlexing
open! Printf

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let newline = [%sedlex.regexp? '\n' | '\r', '\n' | '\r']
let ident_char = [%sedlex.regexp? alphabetic | '_']
let spaces = [%sedlex.regexp? Sub (white_space, Chars "\n\r")]
let hex = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f']
let unicode_escape = [%sedlex.regexp? "\\u", '{', Rep (ascii_hex_digit, 1 .. 5), '}']


let reserved =
  [ "(", OpenParen
  ; ")", CloseParen
  ; "{", OpenBrace
  ; "}", CloseBrace
  ; "toaster", Toaster
  ; "return", Return
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
  ; "==", Equals
  ; "=", Assign
  ; "!=", NotEquals
  ]
  |> List.to_seq
  |> Hashtbl.of_seq


let create_token lexbuf =
  let str = Utf8.lexeme lexbuf in
  Hashtbl.find_opt reserved str |> Option.fold ~none:(Ident str) ~some:id


let create_int lexbuf =
  let str = Utf8.lexeme lexbuf in
  Int (Int64.of_string str)


let parse_hex s =
  let int_of_ch = function
    | '0' .. '9' as c -> int_of_char c - int_of_char '0'
    | 'a' .. 'f' as c -> int_of_char c - int_of_char 'a' + 10
    | 'A' .. 'F' as c -> int_of_char c - int_of_char 'A' + 10
    | _ -> failwith "should not be reachable"
  in
  Seq.fold_left (fun acc ch -> int_of_ch ch + (16 * acc)) 0 s


let escape_sequence lexbuf =
  let str = Utf8.lexeme lexbuf in
  match str with
  | "\\r" -> "\r"
  | "\\n" -> "\n"
  | "\\t" -> "\t"
  | "\\\"" -> "\""
  | _ ->
    begin
      str
      |> String.pop_prefix ~prefix:"\\u{"
      >>= String.pop_suffix ~suffix:"}"
      $> fun hex ->
      let ch = hex |> String.to_seq |> parse_hex |> Uchar.of_int in
      let b = Buffer.create 4 in
      Buffer.add_utf_8_uchar b ch;
      Buffer.contents b
    end
    |> Option.unwrap
         ~exn:(Failure "unrecognized escape sequence. this is a bug. please report this")


(* TODO: implement tokenizer *)
let lex buf : token Seq.t =
  let in_fmt = ref 0 in
  let rec prod ?(after = token) tok = Seq.Cons (tok, fun () -> after buf)
  and token buf =
    let produce tok = Seq.Cons (tok, fun () -> token buf) in
    match%sedlex buf with
    | Plus spaces -> token buf
    | newline ->
      new_line buf;
      token buf
    | "//" -> line_comment buf
    | "/*" -> block_comment 1 buf
    | '"' -> string (Buffer.create 16) buf
    | "f\"" ->
      incr in_fmt;
      prod ~after:(format_string (Buffer.create 16)) BeginString
    | '}' ->
      (* if we see a } it could be that it is closing a fmt context *)
      if !in_fmt = 0
      then produce @@ create_token buf
      else format_string (Buffer.create 16) buf
    | ":>" | ":<" | "->" | "!=" | "==" | Chars "(){+-*/;:,=" ->
      produce @@ create_token buf
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
    | eof -> Seq.Nil
    | _ -> failwith "unreachable state"
  and block_comment n buf =
    match%sedlex buf with
    | "/*" -> block_comment (n + 1) buf
    | "*/" -> if n = 1 then token buf else block_comment (n - 1) buf
    | newline ->
      new_line buf;
      block_comment n buf
    | any -> block_comment n buf
    | eof -> failwith "unterminated block comment"
    | _ -> failwith "unreachable state"
  and string b buf =
    match%sedlex buf with
    | '"' -> Seq.Cons (String (Buffer.contents b), fun () -> token buf)
    | newline ->
      new_line buf;
      Buffer.add_string b (Utf8.lexeme buf);
      string b buf
    | unicode_escape ->

      Buffer.add_string b (escape_sequence buf);
      string b buf
    | '\\', Chars "nrt\"" ->
      Buffer.add_string b (escape_sequence buf);
      string b buf
    | any ->
      Buffer.add_string b (Utf8.lexeme buf);
      string b buf
    | eof -> failwith "unterminated string"
    | _ -> failwith "unreachable state"
  and format_string b buf =
    match%sedlex buf with
    | '"' ->
      (* produce a fragment of the remainder of the string and a EndString token *)
      decr in_fmt;
      prod (StringFragment (Buffer.contents b)) ~after:(const @@ prod EndString)
    | "{{" ->
      Buffer.add_char b '{';
      format_string b buf
    | '{' -> Seq.Cons (StringFragment (Buffer.contents b), fun () -> token buf)
    | newline ->
      new_line buf;
      Buffer.add_string b (Utf8.lexeme buf);
      format_string b buf
    | '}' -> failwith "unopened closing brace (})"
    | unicode_escape ->
      Buffer.add_string b (escape_sequence buf);
      string b buf
    | '\\', Chars "nrt\"" ->
      Buffer.add_string b (escape_sequence buf);
      string b buf
    | any ->
      Buffer.add_string b (Utf8.lexeme buf);
      format_string b buf
    | eof -> failwith "unterminated format string"
    | _ -> failwith "unreachable state"
  in
  Seq.inspect (fun x -> printf "  %s\n" @@ string_of_token x) @@ fun () -> token buf
