type path = Absolute of string list | Relative of string list
module KV : Map.S with type key = string
type query_kv = string KV.t
type t = {
  scheme : string;
  user : string option;
  host : string option;
  port : int option;
  path : path;
  query : string option;
  fragment : string option;
  mutable query_kv : query_kv option;
}

type error =
| Parse_error of string * exn

exception Error of error

val is_absolute : t -> bool
val is_relative : t -> bool

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
val pct_encode_query : string -> string
val path_string : ?pctencode:bool -> t -> string
val to_string : ?pctencode:bool -> t -> string
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
  ?query_kv:string KV.t -> ?query:string -> ?fragment:string -> unit -> t
val scheme : t -> string
val with_scheme : t -> string -> t
val user : t -> string option
val with_user : t -> string option -> t
val host : t -> string option
val with_host : t -> string option -> t
val port : t -> int option
val with_port : t -> int option -> t
val path : t -> path
val with_path : t -> path -> t
val append_path : t -> string list -> t
val query : t -> string option
val with_query : t -> string option -> t
val query_kv : t -> query_kv
val with_query_kv : t -> string KV.t -> t
val query_get : t -> KV.key -> string
val query_opt : t -> KV.key -> string option
val query_set : t -> KV.key -> string -> t
val fragment : t -> string option
val with_fragment : t -> string option -> t
val compare : t -> t -> int
val equal : t -> t -> bool
val normalize_path : string list -> string list
val path_remove_dot_segments : path -> path
val remove_dot_segments : t -> t
val normalize_host : string -> string
val normalize_port : t -> t
val normalize_case : t -> t
val normalize_nfkc : t -> t
val normalize : ?nfkc:bool -> t -> t
val to_uri : t -> string
