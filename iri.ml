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

type error = Iri_lexer.error
exception Error = Iri_lexer.Error
let string_of_error = Iri_lexer.string_of_error

include Iri_types


let of_lexbuf ?(normalize=true) lexbuf =
  let iri = Iri_lexer.iri lexbuf in
  if normalize then Iri_types.normalize iri else iri

let ref_of_lexbuf ?(normalize=false) lexbuf =
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

let ensure_absolute_base ?(normalize=true) ~base iri =
  let resolved =
    match iri with
      Iri iri -> iri
    | Rel iri ->
        let str = to_string iri in
        let len = String.length str in
        if len <= 0 then
          base
        else
          match String.get str 0 with
          | '#' -> with_fragment base (Some (String.sub str 1 (len - 1)))
          | '?' ->
              let base = with_query base None in
              let base = with_fragment base None in
              let s = to_string base ^ str in
              of_string s
          | _ ->
              let base = with_query base None in
              let base = with_fragment base None in
              let path =
                match path iri with
                  Absolute p -> Absolute p
                | Relative iri_path ->
                    let path =
                      match path base with
                        Absolute [] -> Absolute iri_path
                      | Relative [] -> Relative iri_path
                      | Absolute l ->
                          let l = List.rev iri_path @ (List.tl (List.rev l)) in
                          Absolute (List.rev l)
                      | Relative l ->
                          let l = List.rev iri_path @ (List.tl (List.rev l)) in
                          Absolute (List.rev l)
                    in
                    Iri_types.path_remove_dot_segments path
              in
              let i = with_path base path in
              let i = with_fragment i (fragment iri) in
              let i = with_query i (query iri) in
              i
  in
  if normalize then Iri_types.normalize resolved else resolved


