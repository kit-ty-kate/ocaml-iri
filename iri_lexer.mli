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



val pos : ?file:string -> line:int -> bol:int -> char:int -> unit -> Lexing.position
type loc = { loc_start : Lexing.position; loc_stop : Lexing.position; }
type 'a with_loc = 'a * loc option
type error = loc * string
exception Error of error
val error : ?msg: string -> loc -> string -> 'a
val string_of_loc : loc -> string
val string_of_error : loc * string -> string
val loc : Lexing.position -> Lexing.position -> loc
val loc_of_pos : Lexing.position -> int -> loc
val iri : ?pctdecode: bool -> ?pos:Lexing.position -> Sedlexing.lexbuf -> Iri_types.t
val iri_reference : ?pctdecode: bool -> ?pos:Lexing.position -> Sedlexing.lexbuf -> Iri_types.reference
