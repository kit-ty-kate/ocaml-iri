
type path =
| Absolute of string list
| Relative of string list

type iri = {
    scheme : string ;
    user : string option ;
    host : string option ;
    port : int option ;
    path : path ;
    query : string option ; (* not %-decoded as query is not parse to name/value pairs *)
    fragment : string option ;
  }
type iri_reference = Iri of iri | Rel of iri

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

let query_safe_chars =
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

let pct_encode =
  let f is_safe_char b () _i = function
    `Malformed str -> Buffer.add_string b str
  | `Uchar codepoint ->
      if is_safe_char codepoint then
        Uutf.Buffer.add_utf_8 b codepoint
      else
        pct_encode_utf8 b codepoint
  in
  fun is_safe_char s ->
    let b = Buffer.create (String.length s) in
    ignore(Uutf.String.fold_utf_8 (f is_safe_char b) () s);
    Buffer.contents b
;;

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
    (*if has_ihier then Buffer.add_string b "/" ;*)
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

let map_opt f = function None -> None | Some x -> Some (f x)

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
  }

let normalize ?(nfkc=true) t =
  let t = remove_dot_segments t in
  let t = normalize_case t in
  let t = normalize_port t in
  if nfkc then normalize_nfkc t else t


