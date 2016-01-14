
type iri = Iri_types.iri

let of_lexbuf lexbuf = Iri_lexer.iri lexbuf

let of_string str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  of_lexbuf lexbuf

let to_string = Iri_types.to_string ~encode:false
let to_string_encode = Iri_types.to_string ~encode: true