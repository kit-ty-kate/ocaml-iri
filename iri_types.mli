type path = Absolute of string list | Relative of string list
module KV :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
type query_kv = string KV.t
type iri = {
  scheme : string;
  user : string option;
  host : string option;
  port : int option;
  path : path;
  query : string option;
  fragment : string option;
  mutable query_kv : query_kv option;
}
type iri_reference = Iri of iri | Rel of iri
val is_absolute : iri -> bool
val is_relative : iri -> bool
val utf8_nb_bytes_of_char : char -> int
val is_ucschar : int -> bool
val is_iprivate : int -> bool
val pct_decode : string -> string
val safe_chars : bool array
val from_safe_chars : ?f:(int -> bool) -> bool array -> int -> bool
val scheme_safe_chars : bool array
val scheme_safe_char : int -> bool
val sub_delims : char array
val user_safe_chars : bool array
val user_safe_char : Uutf.uchar -> bool
val host_safe_chars : bool array
val host_safe_char : Uutf.uchar -> bool
val path_safe_chars : bool array
val path_safe_char : Uutf.uchar -> bool
val query_part_safe_chars : bool array
val query_part_safe_char : Uutf.uchar -> bool
val fragment_safe_chars : bool array
val fragment_safe_char : Uutf.uchar -> bool
val pct_encode_utf8 : Buffer.t -> Uutf.uchar -> unit
val pct_encode_b : Buffer.t -> (Uutf.uchar -> bool) -> string -> unit
val pct_encode : (Uutf.uchar -> bool) -> string -> string
val to_string : ?encode:bool -> iri -> string
val map_opt : ('a -> 'b) -> 'a option -> 'b option
val utf8_split : (Uutf.uchar -> bool) -> string -> string list
val encode_query_string_part : string -> string
val split_query_string : string -> string KV.t
val split_query_opt : string option -> string KV.t
val query_string_of_kv : ?q:string -> string KV.t -> string
val query_string_of_kv_opt : string KV.t option -> string option
val iri :
  ?scheme:string ->
  ?user:string ->
  ?host:string ->
  ?port:int ->
  ?path:path ->
  ?query_kv:string KV.t -> ?query:string -> ?fragment:string -> unit -> iri
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
val query : iri -> string option
val with_query : iri -> string option -> iri
val query_kv : iri -> query_kv
val with_query_kv : iri -> string KV.t -> iri
val query_get : iri -> KV.key -> string
val query_opt : iri -> KV.key -> string option
val query_set : iri -> KV.key -> string -> iri
val fragment : iri -> string option
val with_fragment : iri -> string option -> iri
val compare : iri -> iri -> int
val ref_to_string : ?encode:bool -> iri_reference -> string
val normalize_path : string list -> string list
val path_remove_dot_segments : path -> path
val remove_dot_segments : iri -> iri
val normalize_host : string -> string
val normalize_port : iri -> iri
val normalize_case : iri -> iri
val normalize_nfkc : iri -> iri
val normalize : ?nfkc:bool -> iri -> iri
