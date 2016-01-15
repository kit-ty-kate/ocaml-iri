type path = Iri_types.path =
| Absolute of string list
| Relative of string list

type iri = Iri_types.iri = {
    scheme : string ;
    user : string option ;
    host : string option ;
    port : int option ;
    path : path ;
    query : string option ;
    fragment : string option ;
  }
type iri_reference = Iri_types.iri_reference =
  Iri of iri | Rel of iri

let is_absolute t =
  match t.fragment with
    None -> t.scheme <> ""
  | _ -> false

let is_relative t = t.scheme = ""

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

let to_string ?encode = Iri_types.to_string ?encode
let ref_to_string ?encode = Iri_types.ref_to_string ?encode


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

let fragment t = t.fragment
let with_fragment t fragment = { t with fragment }

let normalize = Iri_types.normalize
