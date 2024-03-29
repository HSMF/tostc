open Printf
include Fun

let sp = sprintf
let sl map sep lst = lst |> List.map map |> String.concat sep
let ( >>= ) = Option.bind
let ( $> ) op f = Option.map f op
let ( >>> ) f g x = x |> f |> g

module Set = struct
  type 'a t = 'a list

  let rec union xs = function
    | [] -> xs
    | hd :: tl ->
      if List.find_opt (fun x -> x = hd) xs |> Option.is_some
      then union xs tl
      else union (hd :: xs) tl


  let from_list lst =
    let rec helper acc = function
      | [] -> acc
      | hd :: tl -> helper (union acc [ hd ]) tl
    in
    helper [] lst
end

module List = struct
  include List

  let group (n : int) (lst : 'a list) =
    let rec aux acc m lst =
      match lst, acc with
      | [], [] -> []
      | [], x -> [ rev x ]
      | hd :: tl, x ->
        if m = 0 then rev x :: aux [] n (hd :: tl) else aux (hd :: x) (m - 1) tl
    in
    aux [] n lst


  let inspect f lst =
    List.iter f lst;
    lst

  let zip l1 l2 = List.map2 (fun a b -> a, b) l1 l2
end

let sl_grouped n map sep sep_line lst =
  lst |> List.group n |> List.map (sl map sep) |> sl id sep_line


module Option = struct
  include Option

  let unwrap_or default = fold ~none:default ~some:id

  (** fish operator: Provides fallback for option
      [Some x ><> f = Some x]. [None ><> f = f ()] *)
  let ( ><> ) opt or_else =
    match opt with
    | Some x -> Some x
    | None -> or_else ()


  let unwrap ?(exn = Failure "Unwrapped a None value") = function
    | None -> raise exn
    | Some x -> x
end

module Seq = struct
  include Seq

  let inspect f =
    Seq.map (fun x ->
      f x;
      x)
end

module String = struct
  include String

  (** [String.pop_prefix ~prefix s] returns [Some remainder] if [s = prefix ^ remainder] and None otherwise *)
  let pop_prefix ~prefix s =
    if String.starts_with ~prefix s
    then (
      let len = String.length s in
      let prefix_len = String.length prefix in
      Some (String.sub s prefix_len (len - prefix_len)))
    else None


  (** [String.pop_suffix ~suffix s] returns [Some remainder]
      if [s = remainder ^ suffix] and None otherwise *)
  let pop_suffix ~suffix s =
    if String.ends_with ~suffix s
    then (
      let len = String.length s in
      let suffix_len = String.length suffix in
      Some (String.sub s 0 (len - suffix_len)))
    else None
end
