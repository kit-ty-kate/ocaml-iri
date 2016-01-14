


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
