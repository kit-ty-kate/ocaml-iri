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


include Iri_types

let string_of_error = function
| Parse_error (str,e) ->
    let msg =
      match e with
        Iri_lexer.Error e -> Iri_lexer.string_of_error e
      | _ -> Printexc.to_string e
    in
    Printf.sprintf "Parse error in %s\n%s" str msg

let parse_error str e = raise (Error (Parse_error (str, e)))

type iri = t
module Ord = struct type t = iri let compare = Iri_types.compare end
module Set = Set.Make(Ord)
module Map = Map.Make(Ord)

let of_lexbuf ?pctdecode ?pos ?normalize lexbuf =
  let iri = Iri_lexer.iri ?pctdecode ?pos lexbuf in
  match normalize, is_relative iri with
    Some true, _
  | None, false -> Iri_types.normalize iri
  | _ -> iri

let of_string ?pctdecode ?pos ?normalize str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  try of_lexbuf ?pctdecode ?pos ?normalize lexbuf
  with (Iri_lexer.Error _ ) as e -> parse_error str e

let resolve ?(normalize=true) ~base iri =
  let resolved =
    match is_relative iri with
      false -> iri
    | true ->
        let str = to_string iri in
        (*prerr_endline
          (Printf.sprintf "%s=\nscheme=%s\nhost=%s\npath=%s"
           str (scheme iri)
             (match host iri with None -> "" | Some s -> s)
             (path_string iri)
          );*)
        let len = String.length str in
        if len <= 0 then
          base
        else
          match String.get str 0 with
          | '#' -> with_fragment base (fragment iri)
          | '?' ->
              let base = with_fragment base None in
              with_query base (query iri)
          | _ ->
              let base = with_query base None in
              let base = with_fragment base None in
              let base =
                match host iri with
                  None -> base
                | Some host ->
                    let base = with_host base (Some host) in
                    let base = with_port base (port iri) in
                    with_user base (user iri)
              in
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

let parse_http_link str =
  let lexbuf = Sedlexing.Utf8.from_string str in
  Iri_lexer.http_link lexbuf
