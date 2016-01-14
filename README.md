# ocaml-iri
OCaml implementation of Internationalized Resource Identifiers (IRIs)

RFC 3987: http://tools.ietf.org/html/rfc3987

This implementation does not depend on regular expression library.
Is is implemented using Sedlex (https://github.com/alainfrisch/sedlex),
thus it will be usable in Javascript (with Js_of_ocaml).

`OCaml-RDF` will use this library instead of its `Rdf_iri` module. 
