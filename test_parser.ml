open Iri

let iris =
  [ "http://ab.cde123fgh.ij/kl/mn/op.html" ;
    "http:/kl/mn/op.html" ;
    "http:kl/mn/op.html" ;
    "http:?coucou=trois#id" ;
    "ssh://foo@bar.net.com:8080/my/path/?arg=1,42&foo=15#id"
  ]

let () = List.iter
  (fun str ->
     try
       let iri = of_string str in
       print_endline
         (Printf.sprintf "%s -> %s" str (Iri.to_string iri))
     with
     | Iri_lexer.Error e ->
         prerr_endline (Printf.sprintf "%s: %s" str
          (Iri_lexer.string_of_error e))
     | e -> prerr_endline (Printexc.to_string e)
  )
  iris

