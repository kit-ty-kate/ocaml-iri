(*********************************************************************************)
(*                OCaml-IRI                                                      *)
(*                                                                               *)
(*    Copyright (C) 2016 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser General Public           *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

type error
exception Error of error
val string_of_error : error -> string

module KV : Map.S with type key = string
type query_kv = string KV.t

type path =
| Absolute of string list
| Relative of string list

type t
type reference = Iri of t | Rel of t

(** @param query is %-encoded
     @param query_kv is %-decoded *)
val iri :
  ?scheme:string ->
  ?user:string ->
  ?host:string ->
  ?port:int ->
  ?path:path ->
  ?query_kv:query_kv ->
  ?query:string -> ?fragment:string -> unit -> t

module Set : Set.S with type elt = t
module Map : Map.S with type key = t

val is_absolute : t -> bool
val is_relative : t -> bool

val compare : t -> t -> int
val equal : t -> t -> bool

(** Read an IRI from the given string.
  @param normalize tells whether to normalize to IRI or not.
   Default is [true].
   @decode tells whether to %-decode strings or not; default is [true].*)
val of_string : ?pctdecode: bool ->
  ?pos: Lexing.position -> ?normalize:bool -> string -> t
val to_string : ?pctencode: bool -> t -> string

(** Read an IRI reference, i.e. a full IRI or a Relative one.
  @param normalize tells whether to normalize to IRI or not.
   Default is [false] (contrary of {!of_string}) as references will
   usually be resolved from a base IRI.
   @decode tells whether to %-decode strings or not; default is [true].
*)
val ref_of_string : ?pctdecode: bool ->
  ?pos: Lexing.position -> ?normalize:bool -> string -> reference
val ref_to_string : ?pctencode: bool -> reference -> string

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

(** @param encode indicate whether the path elements must be encoded.
  Default is [false]. *)
val path_string : ?pctencode: bool -> t -> string

(** Append the given (not %-encoded) string list to the path of the
     given iri and return a new iri with this path. *)
val append_path : t -> string list -> t

(** Query string is not %-decoded as it is not parsed to name/value pairs *)
val query : t -> string option

(** Key/value pairs from the query string. strings are %-decoded. *)
val query_kv : t -> query_kv

(** Return a new iri with the given optional query string.
  This string must already be %-encoded. *)
val with_query : t -> string option -> t

(** Return a new iri with the given list of key/value pairs.
  The givn string must be %-decoded.
*)
val with_query_kv : t -> query_kv -> t

val query_get : t -> string -> string
val query_opt : t -> string -> string option
val query_set : t -> string -> string -> t

val fragment : t -> string option
val with_fragment : t -> string option -> t

val normalize: ?nfkc:bool -> t -> t

(** @param normalize tells whether to apply normalization after resolution.
     Default is [true]. *)
val resolve : ?normalize: bool -> base: t -> reference -> t
