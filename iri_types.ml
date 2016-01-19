(*********************************************************************************)
(*                OCaml-IRI                                                      *)
(*                                                                               *)
(*    Copyright (C) 2016 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser General Public           *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

type path =
| Absolute of string list
| Relative of string list

module KV = Map.Make(String)
type query_kv = string KV.t

type iri = {
    scheme : string ;
    user : string option ;
    host : string option ;
    port : int option ;
    path : path ;
    query : string option ; (** not %-decoded as query is not parse to name/value pairs *)
    fragment : string option ;
    mutable query_kv : query_kv  option ;
      (** key value pairs from query string to avoid parsing it various times *)
  }
type iri_reference = Iri of iri | Rel of iri

let is_absolute t =
  match t.fragment with
    None -> t.scheme <> ""
  | _ -> false

let is_relative t = t.scheme = ""

let utf8_nb_bytes_of_char c =
  let n = Char.code c in
  if n < 0b10000000 then
    1
  else if n < 0b11100000 then
      2
    else if n < 0b11110000 then
        3
      else
        4
let is_ucschar n =
  (n >= 0xA0 && n <= 0xD7FF) ||
    (n >= 0xF900 && n<= 0xFDCF) ||
    (n >= 0xFDF0 && n <= 0xFFEF) ||
    (n >= 0x10000 && n<= 0x1FFFD) ||
    (n >= 0x20000 && n<= 0x2FFFD) ||
    (n >= 0x30000 && n<= 0x3FFFD) ||
    (n >= 0x40000 && n<= 0x4FFFD) ||
    (n >= 0x50000 && n<= 0x5FFFD) ||
    (n >= 0x60000 && n<= 0x6FFFD) ||
    (n >= 0x70000 && n<= 0x7FFFD) ||
    (n >= 0x80000 && n<= 0x8FFFD) ||
    (n >= 0x90000 && n<= 0x9FFFD) ||
    (n >= 0xA0000 && n<= 0xAFFFD) ||
    (n >= 0xB0000 && n<= 0xBFFFD) ||
    (n >= 0xC0000 && n<= 0xCFFFD) ||
    (n >= 0xD0000 && n<= 0xDFFFD) ||
    (n >= 0xE1000 && n<= 0xEFFFD)
let is_iprivate n =
  (n >= 0xE000 && n <= 0xF8FF) ||
    (n >= 0xF0000 && n <= 0xFFFFD) ||
    (n >= 0x100000 && n <= 0x10FFFD)

let pct_decode =
  let rec iter b len s i =
    if i >= len then
      ()
    else
      begin
        let i =
          match s.[i] with
            '%' when i+2 < len ->
              begin
                try
                  let n = int_of_string ("0x"^(String.sub s (i+1) 2)) in
                  let c = Char.chr n in
                  Buffer.add_char b c;
                  i+3
                with
                  _ -> Buffer.add_char b s.[i]; i+1
              end
          | _ ->
              let size = utf8_nb_bytes_of_char s.[i] in
              if size = 1 then
                Buffer.add_char b s.[i]
              else
                Buffer.add_substring b s i size ;
              i+size
        in
        iter b len s i
      end
  in
  fun s ->
    let len = String.length s in
    let b = Buffer.create len in
    iter b len s 0;
    Buffer.contents b
;;

let safe_chars =
  let f n =
    match Char.chr n with
      'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '.' -> true
    | _ -> false
  in
  Array.init 256 f
;;

let from_safe_chars ?(f=fun _ -> false) safe_chars n =
  assert (n >= 0);
  if n > 255 then f n else safe_chars.(n)

let scheme_safe_chars =
  let a = Array.copy safe_chars in
  a.(Char.code '+') <- true;
  a
;;
let scheme_safe_char = from_safe_chars scheme_safe_chars

let sub_delims =
  [| '!' ; '$' ; '&' ; '\'' ; '(' ; ')' ;
     '*' ; '+' ; ',' ; ';' ; '=' |]
;;

let user_safe_chars =
  let a = Array.copy safe_chars in
  a.(Char.code ':') <- true;
  a.(Char.code '~') <- true;
  a.(Char.code '_') <- true;
  Array.iter (fun c -> a.(Char.code c) <- true) sub_delims ;
  a
;;
let user_safe_char =
  from_safe_chars  ~f: is_ucschar user_safe_chars

let host_safe_chars =
  let a = Array.copy safe_chars in
  a.(Char.code '~') <- true;
  a.(Char.code '_') <- true;
  a.(Char.code '[') <- true;
  a.(Char.code ']') <- true;
  a.(Char.code ':') <- true;
  Array.iter (fun c -> a.(Char.code c) <- true) sub_delims ;
  a
;;
let host_safe_char =
  from_safe_chars ~f: is_ucschar host_safe_chars

let path_safe_chars =
  let a = Array.copy safe_chars in
  a.(Char.code '~') <- true;
  a.(Char.code '_') <- true;
  a.(Char.code ':') <- true;
  a.(Char.code '@') <- true;
  Array.iter (fun c -> a.(Char.code c) <- true) sub_delims ;
  a
;;
let path_safe_char =
  from_safe_chars ~f: is_ucschar path_safe_chars

let query_part_safe_chars =
  let a = Array.copy safe_chars in
  a.(Char.code '~') <- true;
  a.(Char.code '_') <- true;
  a.(Char.code ':') <- true;
  a.(Char.code '@') <- true;
  a.(Char.code '?') <- true;
  a.(Char.code '/') <- true;
  Array.iter (fun c -> a.(Char.code c) <- true) sub_delims ;
  a.(Char.code '=') <- false;
  a.(Char.code '&') <- false;
  a
;;
let query_part_safe_char =
  from_safe_chars ~f: is_ucschar query_part_safe_chars

let fragment_safe_chars =
  let a = Array.copy safe_chars in
  a.(Char.code '~') <- true;
  a.(Char.code '_') <- true;
  a.(Char.code ':') <- true;
  a.(Char.code '@') <- true;
  a.(Char.code '?') <- true;
  a.(Char.code '/') <- true;
  Array.iter (fun c -> a.(Char.code c) <- true) sub_delims ;
  a
;;
let fragment_safe_char =
  from_safe_chars ~f: is_ucschar fragment_safe_chars

let pct_encode_utf8 dest codepoint =
  let b = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 b codepoint;
  let s = Buffer.contents b in
  String.iter
    (fun c -> Printf.bprintf dest "%%%02X" (Char.code c))
    s

let pct_encode_b =
  let f is_safe_char b () _i = function
    `Malformed str -> Buffer.add_string b str
  | `Uchar codepoint ->
      if is_safe_char codepoint then
        Uutf.Buffer.add_utf_8 b codepoint
      else
        pct_encode_utf8 b codepoint
  in
  fun b is_safe_char s ->
    Uutf.String.fold_utf_8 (f is_safe_char b) () s
;;

let pct_encode is_safe_char s =
  let b = Buffer.create (String.length s) in
  pct_encode_b b is_safe_char s ;
  Buffer.contents b

let to_string =
  let string_of_path encode l =
    let l =
      if encode then
        List.map (pct_encode path_safe_char) l
      else
        l
    in
    String.concat "/" l
  in
  fun ?(encode=true) iri  ->
    let has_ihier = iri.host <> None in
    let b = Buffer.create 256 in
    if iri.scheme <> "" then
      (
       Buffer.add_string b iri.scheme ;
       Buffer.add_string b ":"
      );
    if has_ihier then Buffer.add_string b "//";
    (match iri.user with
       None -> ()
     | Some u ->
         Buffer.add_string b
           (if encode then pct_encode user_safe_char u else u);
         Buffer.add_char b '@'
    );
    (match iri.host with
       None -> ()
     | Some s ->
         Buffer.add_string b
           (if encode then pct_encode host_safe_char s else s)
    );
    (match iri.port with None -> () | Some n -> Buffer.add_string b (":"^(string_of_int n))) ;
    Buffer.add_string b
      (match iri.path with
        Absolute l -> "/"^(string_of_path encode l)
      | Relative l -> string_of_path encode l);
    (match iri.query with
       None -> ()
     | Some s ->
         Buffer.add_char b '?';
         Buffer.add_string b s ;
    );
    (match iri.fragment with
       None -> ()
     | Some s ->
         Buffer.add_char b '#' ;
         Buffer.add_string b (if encode then pct_encode fragment_safe_char s else s)
    );
    Buffer.contents b
;;

let map_opt f = function None -> None | Some x -> Some (f x)

let utf8_split =
  let f split_char b_chars acc_words _i = function
    `Malformed str -> Buffer.add_string b_chars str; acc_words
  | `Uchar codepoint ->
      if split_char codepoint then
        begin
          let w = Buffer.contents b_chars in
          Buffer.reset b_chars ;
          match w with
            "" -> acc_words
          | _ -> w :: acc_words
        end
      else
        begin
          Uutf.Buffer.add_utf_8 b_chars codepoint ;
          acc_words
        end
  in
  fun split_char str ->
    let b = Buffer.create 128 in
    let words = Uutf.String.fold_utf_8 (f split_char b) [] str in
    match Buffer.contents b with
      "" -> List.rev words
    | w -> List.rev (w :: words)

let encode_query_string_part = pct_encode query_part_safe_char

let split_query_string =
  let dec = pct_decode in
  let cp_equal = Char.code '=' in
  let cp_amp = Char.code '&' in
  let is_equal cp = cp = cp_equal in
  let is_amp cp = cp = cp_amp in
  let add map str =
    match utf8_split is_equal str with
      [] | [_] -> KV.add (dec str) "" map
    | [ k ; v ] -> KV.add (dec k) (dec v) map
    | k :: vals -> KV.add (dec k) (dec (String.concat "=" vals)) map
  in
  fun str ->
    let l = utf8_split is_amp str in
    List.fold_left add KV.empty l

let split_query_opt = function
| None -> KV.empty
| Some q -> split_query_string q

let query_string_of_kv =
  let f b max_i i (k, v) =
     pct_encode_b b query_part_safe_char k ;
     Buffer.add_char b '=' ;
     pct_encode_b b query_part_safe_char v ;
     if i <> max_i then Buffer.add_char b '&'
  in
  fun ?q pairs ->
    let b = Buffer.create 128 in
    (match q with
       None | Some "" -> ()
     | Some str ->
         Buffer.add_string b str ;
         Buffer.add_char b '&'
    );
    let pairs = KV.bindings pairs in
    List.iteri (f b (List.length pairs - 1)) pairs ;
    Buffer.contents b

let query_string_of_kv_opt = map_opt query_string_of_kv

let iri ?(scheme="") ?user ?host ?port ?(path=Absolute[]) ?query_kv ?query ?fragment () =
  let (query, query_kv) =
    match query, query_kv with
    | None, None -> (None, None)
    | Some _, None -> (query, None)
    | None, Some kv -> (Some (query_string_of_kv kv), query_kv)
    | Some q, Some kv ->
        (* concat both and set kv pairs to none so that it will
           be recreated from the concatenation *)
        (Some (query_string_of_kv ~q kv), None)
  in
  { scheme ; user ; host ; port ; path ; query ; fragment ; query_kv }

let scheme t = t.scheme
let with_scheme t scheme = { t with scheme }

let user t = t.user
let with_user t user = { t with user }

let host t = t.host
let with_host t host = { t with host }

let port t = t.port
let with_port t port = { t with port }

let path t = t.path
let with_path t path = { t with path }

let query t = t.query
let with_query t query = { t with query }

let query_kv t =
  match t.query_kv with
    Some map -> map
  | None ->
      let map = split_query_opt t.query in
      t.query_kv <- Some map;
      map

let with_query_kv t map =
  if KV.is_empty map then
    { t with query = None ; query_kv = None }
  else
    { t with
      query = Some (query_string_of_kv map) ;
      query_kv = Some map ;
    }

let query_get t key =
  match KV.find key (query_kv t) with
  | exception Not_found -> ""
  | str -> str

let query_opt t key =
  match KV.find key (query_kv t) with
  | exception Not_found -> None
  | str -> Some str

let query_set t k v =
  let map = query_kv t in
  let map = KV.add k v map in
  with_query_kv t map

let fragment t = t.fragment
let with_fragment t fragment = { t with fragment }

let compare i1 i2 =
  ignore(query_kv i1);
  ignore(query_kv i2);
  Pervasives.compare i1 i2

let ref_to_string ?encode = function
| Iri iri -> to_string ?encode iri
| Rel iri  -> to_string ?encode iri

let normalize_path =
  let rec iter acc = function
    [] -> List.rev acc
  | "." :: q -> iter acc q
  | ".." :: q ->
      begin
        match acc with
          [] -> iter acc q
        | _ :: acc -> iter acc q
      end
  | [""] -> List.rev (""::acc)
  | "" :: q -> iter acc q
  | h :: q -> iter (h :: acc) q
  in
  iter []
;;

let remove_dot_segments t =
  let path =
    match t.path with
      Absolute l -> Absolute (normalize_path l)
    | Relative l -> Relative (normalize_path l)
  in
  { t with path }

let normalize_host s =
  let len = String.length s in
  if len > 0 then
    match String.get s 0 with
      '[' -> String.uppercase s (* uppercase hexa *)
    | _ -> String.lowercase s (* lowercase regname *)
  else
    s

let normalize_port t =
  match String.lowercase t.scheme, t.port with
    "http", Some 80 -> { t with port = None }
  | "https", Some 443 -> { t with port = None }
  | "ftp", Some 21 -> { t with port = None }
  | "sftp", Some 115 -> { t with port = None }
  | "ssh", Some 22 -> { t with port = None }
  | "smtp", Some 25 -> { t with port = None }
  | _ -> t

let normalize_case t =
  { t with
    scheme = String.lowercase t.scheme ;
    host = map_opt normalize_host t.host ;
  }

let normalize_nfkc t =
  let f = Uunf_string.normalize_utf_8 `NFKC in
  let path =
    match t.path with
      Absolute l -> Absolute (List.map f l)
    | Relative l -> Relative (List.map f l)
  in
  { t with
    host = map_opt f t.host ;
    path ;
    user = map_opt f t.user ;
    query = map_opt f t.query ; (* beware: the query is not %-decoded *)
    fragment = map_opt f t.fragment ;
    query_kv = None ;
  }

let normalize ?(nfkc=true) t =
  let t = remove_dot_segments t in
  let t = normalize_case t in
  let t = normalize_port t in
  if nfkc then normalize_nfkc t else t


