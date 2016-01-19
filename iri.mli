
module KV : Map.S with type key = string
type query_kv = string KV.t

type path =
| Absolute of string list
| Relative of string list

type iri
type iri_reference = Iri of iri | Rel of iri

(** @param query is %-encoded
     @param query_kv is %-decoded *)
val iri :
  ?scheme:string ->
  ?user:string ->
  ?host:string ->
  ?port:int ->
  ?path:path ->
  ?query_kv:query_kv ->
  ?query:string -> ?fragment:string -> unit -> iri

val is_absolute : iri -> bool
val is_relative : iri -> bool

val compare : iri -> iri -> int

(** Read an IRI from the given string.
  @param normalize tells whether to normalize to IRI or not.
   Default is [true].*)
val of_string : ?normalize:bool -> string -> iri
val to_string : ?encode: bool -> iri -> string

val ref_of_string : ?normalize:bool -> string -> iri_reference
val ref_to_string : ?encode: bool -> iri_reference -> string

val scheme : iri -> string
val with_scheme : iri -> string -> iri

val user : iri -> string option
val with_user : iri -> string option -> iri

val host : iri -> string option
val with_host : iri -> string option -> iri

val port : iri -> int option
val with_port : iri -> int option -> iri

val path : iri -> path
val with_path : iri -> path -> iri

(** Query string is not %-decoded as it is not parsed to name/value pairs *)
val query : iri -> string option

(** Key/value pairs from the query string. strings are %-decoded. *)
val query_kv : iri -> query_kv

(** Return a new iri with the given optional query string.
  This string must already be %-encoded. *)
val with_query : iri -> string option -> iri

(** Return a new iri with the given list of key/value pairs.
  The givn string must be %-decoded.
*)
val with_query_kv : iri -> query_kv -> iri

val query_get : iri -> string -> string
val query_opt : iri -> string -> string option
val query_set : iri -> string -> string -> iri


val fragment : iri -> string option
val with_fragment : iri -> string option -> iri

val normalize: ?nfkc:bool -> iri -> iri
