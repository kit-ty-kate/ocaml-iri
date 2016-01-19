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



type pos = { line : int; bol : int; char : int; file : string option; }
val pos : ?file:string -> line:int -> bol:int -> char:int -> unit -> pos
type loc = { loc_start : pos; loc_stop : pos; }
type 'a with_loc = 'a * loc option
type error = loc * string
exception Error of error
val error : ?msg: string -> loc -> string -> 'a
val string_of_loc : loc -> string
val string_of_error : loc * string -> string
val loc : pos -> pos -> loc
val loc_of_pos : pos -> int -> loc
val iri : ?pos:pos -> Sedlexing.lexbuf -> Iri_types.iri
val iri_reference : ?pos:pos -> Sedlexing.lexbuf -> Iri_types.iri_reference
