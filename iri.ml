
include Iri_types

let of_lexbuf ?(normalize=true) lexbuf =
  let iri = Iri_lexer.iri lexbuf in
  if normalize then Iri_types.normalize iri else iri

let ref_of_lexbuf ?(normalize=true) lexbuf =
  let iriref = Iri_lexer.iri_reference lexbuf in
  if normalize then
    match iriref with
      Iri iri -> Iri (Iri_types.normalize iri)
    | Rel iri -> Rel (Iri_types.normalize iri)
  else
    iriref

let of_string ?normalize str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  of_lexbuf ?normalize lexbuf

let ref_of_string ?normalize str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  ref_of_lexbuf ?normalize lexbuf




  