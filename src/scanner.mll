{
  open Parser
  open Lexing
  open Error

  let comment_buf = ref ""

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

rule main = parse
	| "Const" as lxm { incr_bol_lxm lexbuf lxm; CONSTMODIFIER }
  | "Dim"   as lxm { incr_bol_lxm lexbuf lxm; DIMMODIFIER }  
	| "Integer" | "String"   as lxm { incr_bol_lxm lexbuf lxm; VARTYPE lxm }
	| "As" { incr_bol lexbuf 2; DEFTYPE }

  | "If"     { incr_bol lexbuf 2; IFBEGIN }
	| "Then"   { incr_bol lexbuf 4; IFTHEN }
  | "End If" { incr_bol lexbuf 6; IFEND }  

	| identifier as lxm { incr_bol_lxm lexbuf lxm; IDENTIFIER lxm }
	| integer    as lxm { incr_bol_lxm lexbuf lxm; INTEGER lxm }
	| string     as lxm { incr_bol_lxm lexbuf lxm; STR lxm }

  | "=" { incr_bol lexbuf 1; EQUAL }
  | "<" { incr_bol lexbuf 1; GTHAN }
	| ">" { incr_bol lexbuf 1; LTHAN }
  | "'" { comment_buf := ""; comment lexbuf }

	| ws   { incr_bol lexbuf 1; main lexbuf }
	| '\n' { incr_line lexbuf; EOL }
	| eof  { EOF }

  | _ as c { Error.error ("Unrecognized character " ^ (String.make 1 c)) lexbuf.lex_curr_p }

and comment = parse
  | '\n' | eof { incr_line lexbuf; COMMENT !comment_buf }
  | _  as lxm { comment_buf := (!comment_buf ^ (String.make 1 lxm)); incr_bol lexbuf 1; comment lexbuf }

