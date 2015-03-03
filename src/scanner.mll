{
  open Parser
  open Lexing
  open Error

  (** Increments the lexing buffer line number counter.*)
  let incr_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {pos with pos_lnum = pos.pos_lnum + 1; pos_bol = 0}
  
  (** Increments the lexing buffer line offset by the given length. *)
  let incr_bol lexbuf length =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- {pos with pos_bol = pos.pos_bol + length}
    
  (** Increments the lexing buffer line offset by the given lexem length. *)
  let incr_bol_lxm lexbuf lxm = incr_bol lexbuf (String.length lxm)  
}

let identifier = ['a'-'z' 'A'-'Z' '_']+
let integer = ['0'-'9']+
let string = '"' [^'"']* '"'
let ws = [' ' '\t']
(** let eol = ['\r' '\n'] **)

rule main = parse
	| "Const" | "Dim" as lxm { incr_bol_lxm lexbuf lxm; MODIFIER lxm }
	| "Integer" | "String"   as lxm { incr_bol_lxm lexbuf lxm; VARTYPE lxm }
	| "As"		 { incr_bol lexbuf 1; DEFTYPE }
	
	| identifier as lxm { incr_bol_lxm lexbuf lxm; IDENTIFIER lxm }
	| integer    as lxm { incr_bol_lxm lexbuf lxm; INTEGER lxm }
	| string     as lxm { incr_bol lexbuf ((String.length lxm) - 2); STR (String.sub lxm 1 ((String.length lxm) - 2)) }

	| "=" { incr_bol lexbuf 1; EQUAL }

	| ws  { incr_bol lexbuf 1; main lexbuf }
	| '\n' { incr_line lexbuf; EOL }
	| eof { EOF }

  	| _ as c { Error.error ("Unrecognized character " ^ (String.make 1 c)) lexbuf.lex_curr_p }
