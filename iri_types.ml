
type path =
| Absolute of string list
| Relative of string list
type iri = {
    scheme : string ;
    user : string option ;
    host : string option ;
    port : int option ;
    path : path ;
    query : string option ;
    fragment : string option ;
  }
type iri_reference = Iri of iri | Rel of iri
exception Parse_error

let safe_chars =
  let f n =
    match Char.chr n with
      'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '.' | '%' -> true
    | _ -> false
  in
  Array.init 256 f
;;
let scheme_safe_chars =
  let a = Array.copy safe_chars in
  a.(Char.code '+') <- true;
  a
;;

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

let path_safe_chars =
  let a = Array.copy safe_chars in
  a.(Char.code '~') <- true;
  a.(Char.code '_') <- true;
  a.(Char.code ':') <- true;
  a.(Char.code '@') <- true;
  Array.iter (fun c -> a.(Char.code c) <- true) sub_delims ;
  a
;;

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

let pct_encode =
  let rec iter safe_chars b len s i =
   if i >= len then
      ()
    else
      begin
        if safe_chars.(Char.code s.[i]) then
          Buffer.add_char b s.[i]
        else
          begin
            Printf.bprintf b "%%%02X" (Char.code s.[i])
          end;
        iter safe_chars b len s (i+1)
      end
  in
  fun safe_chars s ->
    let len = String.length s in
    let b = Buffer.create len in
    iter safe_chars b len s 0;
    Buffer.contents b
;;

let to_string =
  let string_of_path encode l =
    let l =
      if encode then
        List.map (pct_encode path_safe_chars) l
      else
        l
    in
    String.concat "/" l
  in
  fun ?(encode=false) iri  ->
    let has_ihier = iri.host <> None in
    let b = Buffer.create 256 in
    Buffer.add_string b iri.scheme ;
    Buffer.add_string b ":" ;
    if has_ihier then Buffer.add_string b "//";
    (match iri.user with
       None -> ()
     | Some u ->
         Buffer.add_string b
           (if encode then pct_encode user_safe_chars u else u);
         Buffer.add_char b '@'
    );
    (match iri.host with
       None -> ()
     | Some s ->
         Buffer.add_string b
           (if encode then pct_encode host_safe_chars s else s)
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
         Buffer.add_string b (if encode then pct_encode query_safe_chars s else s)
    );
    (match iri.fragment with
       None -> ()
     | Some s ->
         Buffer.add_char b '#' ;
         Buffer.add_string b (if encode then pct_encode fragment_safe_chars s else s)
    );
    Buffer.contents b
;;
