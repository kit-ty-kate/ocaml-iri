open Iri

let prerr_endline =
  if Unix.isatty Unix.stderr then
    fun str -> prerr_endline
      (Printf.sprintf "\027[1;31m%s\027[0m" str)
  else
    prerr_endline

let test_string str =
  try
    let iri = ref_of_string str in
    let str2 = Iri.ref_to_string iri in
    print_endline
      (Printf.sprintf "%s -> %s %s%s" str str2
       (match iri with
        | Rel _ -> ""
        | Iri iri ->
            match Iri.query iri with
            | None -> ""
            | Some _ ->
                let map = Iri.query_kv iri in
                String.concat ", "
                  ((List.map
                    (fun (k, v) -> Printf.sprintf "%S->%S" k v))
                   (Iri.KV.bindings map))
       )
      (try
         let iri = Iri.ref_of_string str2 in
         let str3 = Iri.ref_to_string iri in
         if str2 = str3 then
           ""
         else
           (Printf.sprintf " [not reparsed the same way str3=%s]" str3)
       with
       | e ->
           let msg =
             match e with
             | Iri_lexer.Error e ->
                 Printf.sprintf "%s"  (Iri_lexer.string_of_error e)
             | e -> Printexc.to_string e
           in
           Printf.sprintf " [reparse failed: %s]" msg
      ))
  with
  | Iri_lexer.Error e ->
      prerr_endline (Printf.sprintf "%s: %s" str
       (Iri_lexer.string_of_error e))
  | e -> prerr_endline (Printexc.to_string e)

let test_mine () =
  let iris =
    [ "urn:example:mammal:monotreme:echidna";
      "http://ab.cde123fgh.ij/kl/mn/op.html" ;
      "http:/kl/mn/op.html" ;
      "http:kl/mn/op.html" ;
      "http:?coucou=trois#id" ;
      "ssh://foo@bar.net.com:8080/my/path/?arg=1%3D4%32&foo=15#id";
      "http://ab.גדהוזח.ij/kl/mn/op.html";
      "http://www.ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.w3.mag.keio.ac.jp/";
      "http://ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.w3.mag.keio.ac.jp/";
    ]
  in
  List.iter test_string iris

let test_file file =
  let ic = open_in file in
  (try while true do test_string (input_line ic) done
   with End_of_file -> ()
  );
  close_in ic

let () =
  if Array.length Sys.argv > 1 then
    Array.iteri (fun i f -> if i > 0 then test_file f) Sys.argv
  else
    test_mine ()


