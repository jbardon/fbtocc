{
  open Parser
}

let func = ['a'-'z' 'A'-'Z' '_']+
let string = '"' [^'"']* '"'
let ws = [' ' '\t']
let eol = ['\r' '\n']

rule main = parse
	| func   as fname {FUNC fname}
	| string as str   {STR str}

	| ws  {main lexbuf}
	| eol {EOL}
	| eof {EOF}

	| _ as c {CHAR c}
