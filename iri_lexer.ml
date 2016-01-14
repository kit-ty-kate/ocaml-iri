
(* Core rules from ABNF http://tools.ietf.org/html/rfc2234 *)

let digit = [%sedlex.regexp? '0'..'9']
let alpha = [%sedlex.regexp? 'a'..'z'|'A'..'Z']
let bit = [%sedlex.regexp? '0' | '1']
let char = [%sedlex.regexp? 0x01..0x7F]
  (* any 7-bit US-ASCII character, excluding NUL *)

let cr = [%sedlex.regexp? 0x0D] (* carriage return *)
let lf = [%sedlex.regexp? 0x0A] (* line feed *)
let crlf = [%sedlex.regexp? cr,lf] (* Internet standard newline *)
let ctl = [%sedlex.regexp? 0x00..0x1F | 0x7F] (* controls *)
let dquote = [%sedlex.regexp? '"'] (* Double Quote, \x22 *)
let hexdig = [%sedlex.regexp? digit | 'A'..'F']
let htab = [%sedlex.regexp? 0x09] (* horizontal tab *)
let sp = [%sedlex.regexp? ' '] (* space, \x20 *)
let wsp = [%sedlex.regexp? sp | htab] (* white space *)
let lwsp = [%sedlex.regexp? Star((wsp | crlf), wsp)]
   (* linear white space (past newline) *)

let octect = [%sedlex.regexp? 0x00..0xFF] (* 8 bits of data *)
let vchar = [%sedlex.regexp? 0x21..0x7E]
  (* visible (printing) characters *)

(* tools to handle locations in lexbuf *)

type pos = { line: int ; bol : int; char: int ; file : string option }
let pos ?file ~line ~bol ~char () = { line ; bol ; char ; file }

type loc = { loc_start: pos; loc_stop: pos }
type 'a with_loc = 'a * loc option

type error = loc * string
exception Error of error
let error ?(msg="Parse error") loc = raise (Error (loc, msg))

let string_of_loc loc =
  let start = loc.loc_start in
  let stop = loc.loc_stop in
  let line = start.line in
  let char = start.char - start.bol in
  let len =
    if start.file = stop.file then
      stop.char - start.char
    else
      1
  in
  let file = start.file in
  Printf.sprintf "%sline %d, character%s %d%s"
    (match file with
     | None -> ""
     | Some s -> Printf.sprintf "File %S, " s)
    line
    (if len > 1 then "s" else "")
    char
    (if len > 1 then Printf.sprintf "-%d" (char + len) else "")

let loc_sprintf loc fmt =
  match loc with
  | None -> Printf.sprintf fmt
  | Some loc -> Printf.ksprintf
      (fun s -> Printf.sprintf "%s:\n%s" (string_of_loc loc) s)
      fmt

let string_of_error (loc, str) =
  Printf.sprintf "%s: %s" (string_of_loc loc) str

let loc loc_start loc_stop = { loc_start ; loc_stop }
let loc_of_pos pos len =
  { loc_start = pos ;
    loc_stop = { pos with char = pos.char + len } ;
  }
let error_pos ?msg pos = error ?msg (loc_of_pos pos 1)

let nl_code = Char.code '\n'

let update_pos pos str =
  let f pos i = function
  | `Malformed msg -> error ~msg (loc_of_pos pos 1)
  | `Uchar c when c = nl_code ->
      let bol = pos.char in
      { pos with
        line = pos.line + 1;
        bol ;
        char = pos.char + 1 ;
      }
  | _ -> { pos with char = pos.char + 1}
  in
  Uutf.String.fold_utf_8 f pos str


(* rules from IRI RFC *)

let ucschar = [%sedlex.regexp?
    0xA0..0xD7FF | 0xF900..0xFDCF | 0xFDF0..0xFFEF
  | 0x10000..0x1FFFD | 0x20000..0x2FFFD | 0x30000..0x3FFFD
  | 0x40000..0x4FFFD | 0x50000..0x5FFFD | 0x60000..0x6FFFD
  | 0x70000..0x7FFFD | 0x80000..0x8FFFD | 0x90000..0x9FFFD
  | 0xA0000..0xAFFFD | 0xB0000..0xBFFFD | 0xC0000..0xCFFFD
  | 0xD0000..0xDFFFD | 0xE1000..0xEFFFD ]

let iprivate = [%sedlex.regexp?
    0xE000..0xF8FF | 0xF0000..0xFFFFD | 0x100000..0x10FFFD]

open Iri_types
module L = Sedlexing.Utf8

let upd pos lexbuf = update_pos pos (L.lexeme lexbuf)

let iunreserved = [%sedlex.regexp? alpha | digit | Chars "-._~" | ucschar]
let pct_encoded = [%sedlex.regexp? '%', hexdig, hexdig ]
let gen_delims = [%sedlex.regexp? Chars ":/?#[]@"]
let sub_delims = [%sedlex.regexp? Chars "!$&'()+,;="]
let iuserinfo = [%sedlex.regexp? Star(iunreserved|pct_encoded|sub_delims|':')]

let unreserved = [%sedlex.regexp? alpha|digit|Chars "-._~"]
let dec_octet = [%sedlex.regexp?
    digit | ('1'..'9',digit) | ('1',digit,digit) | ('2','0'..'4',digit) | ("25",'0'..'5')]
let ipv4address = [%sedlex.regexp? dec_octet,'.',dec_octet,'.',dec_octet,'.',dec_octet]
let ipvfuture = [%sedlex.regexp? 'v', Plus(hexdig), '.', Plus(unreserved|sub_delims|':')]

let h16 = [%sedlex.regexp?
    hexdig | (hexdig,hexdig) | (hexdig,hexdig,hexdig) | (hexdig,hexdig,hexdig,hexdig) ]
let ls32 = [%sedlex.regexp? (h16, ':', h16) | ipv4address]
let ipv6address = [%sedlex.regexp?
    ( h16,':',h16,':',h16,':',h16,':',h16,':',h16,':',ls32 )
  | ("::" ,h16,':',h16,':',h16,':',h16,':',h16,':',ls32)
  | (h16,"::",h16,':',h16,':',h16,':',h16,':',ls32)
  | (h16,':',h16,"::",h16,':',h16,':',h16,':',ls32)
  | (h16,':',h16,':',h16,"::",h16,':',h16,':',ls32)
  | (h16,':',h16,':',h16,':',h16,"::",h16,':',ls32)
  | (h16,':',h16,':',h16,':',h16,':',h16,"::",ls32)
  | (h16,':',h16,':',h16,':',h16,':',h16,':',h16,"::",h16)
  | (h16,':',h16,':',h16,':',h16,':',h16,':',h16,':',h16,"::")
]

let ip_literal = [%sedlex.regexp? '[', (ipv6address|ipvfuture), ']']
let ireg_name = [%sedlex.regexp? Star(iunreserved|pct_encoded|sub_delims)]
let ihost = [%sedlex.regexp? ip_literal | ipv4address | ireg_name]
let port = [%sedlex.regexp? Star(digit)]

let ipchar = [%sedlex.regexp? iunreserved|pct_encoded|sub_delims|':'|'@']

let fragment_opt pos lexbuf =
  match%sedlex lexbuf with
    '#', Star(ipchar|'/'|'?') ->
      let str = L.lexeme lexbuf in
      let len = String.length str in
      let pos = upd pos lexbuf in
      (pos, Some (String.sub str 1 (len-1)))
  | '?', any ->
      error_pos pos
  | _ ->
      Sedlexing.rollback lexbuf;
      (pos, None)

let query_opt pos lexbuf =
  match%sedlex lexbuf with
    '?', Star(ipchar|iprivate|'/'|'?') ->
      let str = L.lexeme lexbuf in
      let len = String.length str in
      let pos = upd pos lexbuf in
      (pos, Some (String.sub str 1 (len-1)))
  | '?', any ->
      error_pos pos
  | _ ->
      Sedlexing.rollback lexbuf;
      (pos, None)

let rec isegment_list acc pos lexbuf =
  match%sedlex lexbuf with
    '/', Star(ipchar) ->
      let str = L.lexeme lexbuf in
      let len = String.length str in
      let pos = upd pos lexbuf in
      isegment_list ((String.sub str 1 (len-1)) :: acc) pos lexbuf
  | _ ->
      Sedlexing.rollback lexbuf ;
      (pos, List.rev acc)

let ipath_abempty pos lexbuf =
  let (pos, path) = isegment_list [] pos lexbuf in
  (pos, Absolute path)

let iauthority pos lexbuf =
  match%sedlex lexbuf with
    ihost, ':', port ->
      begin
        let str = L.lexeme lexbuf in
        let len = String.length str in
        let p = String.rindex str ':' in
        let port =
          match String.sub str (p+1) (len - p - 1) with
            "" -> None
          | s -> Some (int_of_string s)
        in
        let host = String.sub str 0 p in
        let pos = upd pos lexbuf in
        (pos, host, port)
      end
  | ihost ->
      let host = L.lexeme lexbuf in
      let pos = upd pos lexbuf in
      (pos, host, None)
  | _ ->
      error_pos pos

let iauthority_with_user pos lexbuf =
  match%sedlex lexbuf with
  | iuserinfo, '@' ->
      let str = L.lexeme lexbuf in
      let len = String.length str in
      let user = String.sub str 0 (len - 1) in
      let pos = upd pos lexbuf in
      let (pos, h, p) = iauthority pos lexbuf in
      (pos, Some user, h, p)
  | _ ->
      Sedlexing.rollback lexbuf ;
      let (pos, h, p) = iauthority pos lexbuf in
      (pos, None, h, p)

let ihier_part pos lexbuf =
  match%sedlex lexbuf with
    "//" ->
      let pos = upd pos lexbuf in
      let (pos, u, h, p) = iauthority_with_user pos lexbuf in
      let (pos, path) = ipath_abempty pos lexbuf in
      (pos, u, Some h, p, path)
  | '/', Plus(ipchar) -> (*ipath-absolute *)
      let str = L.lexeme lexbuf in
      let len = String.length str in
      let str = String.sub str 1 (len - 1) in
      let pos = upd pos lexbuf in
      let (pos, path) = isegment_list [str] pos lexbuf in
      (pos, None, None, None, Absolute path)
  | Plus(ipchar) -> (* ipath_rootless *)
      let str = L.lexeme lexbuf in
      let pos = upd pos lexbuf in
      let (pos, path) = isegment_list [str] pos lexbuf in
      (pos, None, None, None, Relative path)
  | _ ->
      Sedlexing.rollback lexbuf ;
      (pos, None, None, None, Relative [])

let iri ?(pos=pos ~line: 1 ~bol: 0 ~char: 1 ()) lexbuf =
  match%sedlex lexbuf with
    alpha, Star(alpha|digit|Chars"+-."), ':' ->
      let str = L.lexeme lexbuf in
      let len = String.length str in
      let scheme = String.sub str 0 (len - 1) in
      let pos = upd pos lexbuf in
      let (pos, user, host, port, path) = ihier_part pos lexbuf in
      let (pos, query) = query_opt pos lexbuf in
      let (pos, fragment) = fragment_opt pos lexbuf in
      { scheme ; user ; host ; port ; path ;
        query; fragment;
      }
  |  _ ->
      let pos = upd pos lexbuf in
      error_pos pos
(*
let lexer lexbuf =
  let buf = lexbuf.L.stream in
  match%sedlex buf with
    '[' -> L.update lexbuf; LBRACKET
  | ']' -> L.update lexbuf; RBRACKET
  | '-' -> L.update lexbuf; MINUS
  | '.' -> L.update lexbuf; DOT
  | '_' -> L.update lexbuf; UNDERSCORE
  | '~' -> L.update lexbuf; TILDE
  | ':' -> L.update lexbuf; COLON
  | '/' -> L.update lexbuf; SLASH
  | '?' -> L.update lexbuf; QMARK
  | '#' -> L.update lexbuf; SHARP
  | '@' -> L.update lexbuf; AROBAS
  | '!' -> L.update lexbuf; BANG
  | '$' -> L.update lexbuf; DOLLAR
  | '&' -> L.update lexbuf; AMPERSAND
  | '\'' -> L.update lexbuf; QUOTE
  | '(' -> L.update lexbuf; LPAR
  | ')' -> L.update lexbuf; RPAR
  | '*' -> L.update lexbuf; STAR
  | '+' -> L.update lexbuf; PLUS
  | ',' -> L.update lexbuf; COMMA
  | ';' -> L.update lexbuf; SEMICOLON
  | '=' -> L.update lexbuf; EQUAL
  | '%' -> L.update lexbuf; PERCENT
  | 'A' -> L.update lexbuf; UA (L.lexeme lexbuf)
  | 'B' -> L.update lexbuf; UB (L.lexeme lexbuf)
  | 'C' -> L.update lexbuf; UC (L.lexeme lexbuf)
  | 'D' -> L.update lexbuf; UD (L.lexeme lexbuf)
  | 'E' -> L.update lexbuf; UE (L.lexeme lexbuf)
  | 'F' -> L.update lexbuf; UF (L.lexeme lexbuf)
  | 'G' -> L.update lexbuf; UG (L.lexeme lexbuf)
  | 'H' -> L.update lexbuf; UH (L.lexeme lexbuf)
  | 'I' -> L.update lexbuf; UI (L.lexeme lexbuf)
  | 'J' -> L.update lexbuf; UJ (L.lexeme lexbuf)
  | 'K' -> L.update lexbuf; UK (L.lexeme lexbuf)
  | 'L' -> L.update lexbuf; UL (L.lexeme lexbuf)
  | 'M' -> L.update lexbuf; UM (L.lexeme lexbuf)
  | 'N' -> L.update lexbuf; UN (L.lexeme lexbuf)
  | 'O' -> L.update lexbuf; UO (L.lexeme lexbuf)
  | 'P' -> L.update lexbuf; UP (L.lexeme lexbuf)
  | 'Q' -> L.update lexbuf; UQ (L.lexeme lexbuf)
  | 'R' -> L.update lexbuf; UR (L.lexeme lexbuf)
  | 'S' -> L.update lexbuf; US (L.lexeme lexbuf)
  | 'T' -> L.update lexbuf; UT (L.lexeme lexbuf)
  | 'U' -> L.update lexbuf; UU (L.lexeme lexbuf)
  | 'V' -> L.update lexbuf; UV (L.lexeme lexbuf)
  | 'W' -> L.update lexbuf; UW (L.lexeme lexbuf)
  | 'X' -> L.update lexbuf; UX (L.lexeme lexbuf)
  | 'Y' -> L.update lexbuf; UY (L.lexeme lexbuf)
  | 'Z' -> L.update lexbuf; UZ (L.lexeme lexbuf)
  | 'a' -> L.update lexbuf; A (L.lexeme lexbuf)
  | 'b' -> L.update lexbuf; B (L.lexeme lexbuf)
  | 'c' -> L.update lexbuf; C (L.lexeme lexbuf)
  | 'd' -> L.update lexbuf; D (L.lexeme lexbuf)
  | 'e' -> L.update lexbuf; E (L.lexeme lexbuf)
  | 'f' -> L.update lexbuf; F (L.lexeme lexbuf)
  | 'g' -> L.update lexbuf; G (L.lexeme lexbuf)
  | 'h' -> L.update lexbuf; H (L.lexeme lexbuf)
  | 'i' -> L.update lexbuf; I (L.lexeme lexbuf)
  | 'j' -> L.update lexbuf; J (L.lexeme lexbuf)
  | 'k' -> L.update lexbuf; K (L.lexeme lexbuf)
  | 'l' -> L.update lexbuf; L (L.lexeme lexbuf)
  | 'm' -> L.update lexbuf; M (L.lexeme lexbuf)
  | 'n' -> L.update lexbuf; N (L.lexeme lexbuf)
  | 'o' -> L.update lexbuf; O (L.lexeme lexbuf)
  | 'p' -> L.update lexbuf; P (L.lexeme lexbuf)
  | 'q' -> L.update lexbuf; Q (L.lexeme lexbuf)
  | 'r' -> L.update lexbuf; R (L.lexeme lexbuf)
  | 's' -> L.update lexbuf; S (L.lexeme lexbuf)
  | 't' -> L.update lexbuf; T (L.lexeme lexbuf)
  | 'u' -> L.update lexbuf; U (L.lexeme lexbuf)
  | 'v' -> L.update lexbuf; V (L.lexeme lexbuf)
  | 'w' -> L.update lexbuf; W (L.lexeme lexbuf)
  | 'x' -> L.update lexbuf; X (L.lexeme lexbuf)
  | 'y' -> L.update lexbuf; Y (L.lexeme lexbuf)
  | 'z' -> L.update lexbuf; Z (L.lexeme lexbuf)

  | digit -> L.update lexbuf; Digit (L.lexeme lexbuf)
  | iprivate -> L.update lexbuf; Iprivate (L.lexeme lexbuf)
  | ucschar -> L.update lexbuf; Ucschar (L.lexeme lexbuf)
  | eof -> L.update lexbuf; EOF
  | _ -> L.update lexbuf; L.raise_ParseError lexbuf
*)












                  