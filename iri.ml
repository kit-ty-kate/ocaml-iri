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

let of_lexbuf lexbuf = Iri_lexer.iri lexbuf

let of_string str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  of_lexbuf lexbuf

let to_string ?encode = Iri_types.to_string ?encode

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


