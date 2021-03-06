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

open Iri

let prerr_endline =
  if Unix.isatty Unix.stderr then
    fun str -> prerr_endline
      (Printf.sprintf "\027[1;31m%s\027[0m" str)
  else
    prerr_endline

let test_string str =
  try
    let iri = Iri.of_string str in
    let str2 = Iri.to_string iri in
    print_endline
      (Printf.sprintf "%s -> %s %s%s" str str2
       (
        match Iri.query iri with
        | None -> ""
        | Some _ ->
            let map = Iri.query_kv iri in
            String.concat ", "
              ((List.map
                (fun (k, v) -> Printf.sprintf "%S->%S" k v))
               (Iri.KV.bindings map))
       )
      (try
         let iri = Iri.of_string str2 in
         let str3 = Iri.to_string iri in
         if str2 = str3 then
           ""
         else
           (Printf.sprintf " [not reparsed the same way str3=%s]" str3)
       with
       | e ->
           let msg =
             match e with
             | Iri_lexer.Error e ->
                 Printf.sprintf "%s"  (Iri_lexer.string_of_error e)
             | Iri.Error e ->  Iri.string_of_error e
             | e -> Printexc.to_string e
           in
           Printf.sprintf " [reparse failed: %s]" msg
      ))
  with
  | Iri_lexer.Error e ->
      prerr_endline (Printf.sprintf "%s: %s" str
       (Iri_lexer.string_of_error e))
  | Iri.Error e ->  prerr_endline (Iri.string_of_error e)
  | e -> prerr_endline (Printexc.to_string e)

let test_mine () =
  let iris =
    [ "" ;
      "urn:example:mammal:monotreme:echidna";
      "http://ab.cde123fgh.ij/kl/mn/op.html" ;
      "http:/kl/mn/op.html" ;
      "http:kl/mn/op.html" ;
      "http:?coucou=trois#id" ;
      "http://a/b/c/d/?coucou=trois#id" ;
      "http://a/b/c/d?coucou=trois#id" ;
      "ssh://foo@bar.net.com:8080/my/path/?arg=1%3D4%32&foo=15#id";
      "http://ab.גדהוזח.ij/kl/mn/op.html";
      "http://www.ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.w3.mag.keio.ac.jp/";
      "http://ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.w3.mag.keio.ac.jp/";
    ]
  in
  List.iter test_string iris

let test_ref_resolve () =
  let base = Iri.of_string "http://a/b/c/d;p?q" in
  let test (str, expected) =
    try
      let expected = Iri.of_string expected in
      let r = Iri.of_string str in
      let resolved = Iri.resolve ~normalize: true ~base r in
      let ok = Iri.to_string resolved = Iri.to_string expected in
      let msg =
        Printf.sprintf "%s + %s => %s [%s]"
          (Iri.to_string base)
          (Iri.to_string r)
          (Iri.to_string resolved)
          (if ok then "OK" else Printf.sprintf "KO, expected %s" (Iri.to_string expected))
      in
      if ok then print_endline msg else prerr_endline msg
    with
      Iri.Error e ->
        let msg = Printf.sprintf "%s, %s: %s" expected str (Iri.string_of_error e) in
        prerr_endline msg
  in
  List.iter test
    [
      "g:h"       ,  "g:h" ;
      "g"         ,  "http://a/b/c/g" ;
      "./g"       ,  "http://a/b/c/g" ;
      "g/"        ,  "http://a/b/c/g/" ;
      "/g"        ,  "http://a/g" ;
      "//g"       ,  "http://g" ;
      "?y"        ,  "http://a/b/c/d;p?y" ;
      "g?y"       ,  "http://a/b/c/g?y" ;
      "#s"        ,  "http://a/b/c/d;p?q#s" ;
      "g#s"       ,  "http://a/b/c/g#s" ;
      "g?y#s"     ,  "http://a/b/c/g?y#s" ;
      ";x"        ,  "http://a/b/c/;x" ;
      "g;x"       ,  "http://a/b/c/g;x" ;
      "g;x?y#s"   ,  "http://a/b/c/g;x?y#s" ;
      ""          ,  "http://a/b/c/d;p?q" ;
      "."         ,  "http://a/b/c/" ;
      "./"        ,  "http://a/b/c/" ;
      ".."        ,  "http://a/b/" ;
      "../"       ,  "http://a/b/" ;
      "../g"      ,  "http://a/b/g" ;
      "../.."     ,  "http://a/" ;
      "../../"    ,  "http://a/" ;
      "../../g"   ,  "http://a/g" ;
      "../../../g",  "http://a/g" ;
      "../../../../g",  "http://a/g" ;
       "/./g"    , "http://a/g" ;
      "/../g"    , "http://a/g" ;
      "g."       , "http://a/b/c/g." ;
      ".g"       , "http://a/b/c/.g" ;
      "g.."      , "http://a/b/c/g.." ;
      "..g"      , "http://a/b/c/..g" ;
       "./../g"    ,  "http://a/b/g" ;
      "./g/."     ,  "http://a/b/c/g/" ;
      "g/./h"     ,  "http://a/b/c/g/h" ;
      "g/../h"    ,  "http://a/b/c/h" ;
      "g;x=1/./y" ,  "http://a/b/c/g;x=1/y" ;
      "g;x=1/../y",  "http://a/b/c/y" ;
      "g?y/./x"   ,  "http://a/b/c/g?y/./x" ;
      "g?y/../x"  ,  "http://a/b/c/g?y/../x" ;
      "g#s/./x"   ,  "http://a/b/c/g#s/./x" ;
      "g#s/../x"  ,  "http://a/b/c/g#s/../x" ;
      "http:g"    ,  "http:g" ;
    ]

let test_http_link () =
  let f str =
    let l = Iri.parse_http_link str in
    let b = Buffer.create 256 in
    Printf.bprintf b "Link: %s\n" str ;
    List.iter
      (fun (s, iri) ->
         Printf.bprintf b "=> %s rel=%S\n" (Iri.to_string iri) s)
      l;
    print_endline (Buffer.contents b)
  in
  List.iter f
    [ {|<http://www.w3.org/ns/ldp#BasicContainer>; rel="type", <http://www.w3.org/ns/ldp#Resource>; rel="type" |} ;
      {|  <http://www.w3.org/ns/ldp#IndirectContainer>; rel="type",  <http://www.w3.org/ns/ldp#Resource>; rel="type"|} ;
    ]


let test_file file =
  let ic = open_in file in
  (try while true do test_string (input_line ic) done
   with End_of_file -> ()
  );
  close_in ic

let () =
  if Array.length Sys.argv > 1 then
    Array.iteri (fun i f -> if i > 0 then test_file f) Sys.argv
  else
    (test_ref_resolve () ; test_mine () ; test_http_link ())


