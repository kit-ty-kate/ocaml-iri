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

val to_string : ?encode:bool -> iri -> string
val ref_to_string : ?encode:bool -> iri_reference -> string

val pct_decode : string -> string
val map_opt : ('a -> 'b) -> 'a option -> 'b option

val normalize: ?nfkc:bool -> iri -> iri