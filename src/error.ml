open Lexing 

type lexing_exception = string * Lexing.position
exception LexingError of lexing_exception

let error message pos = raise (LexingError (message, pos))

let print (m,p) =
  Printf.eprintf "Error line %d character %d: %s\n" p.pos_lnum (p.pos_bol + 1) m