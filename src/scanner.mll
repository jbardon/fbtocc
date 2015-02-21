{
  open Parser
}

let identifier = ['a'-'z' 'A'-'Z' '_']+
let integer = ['0'-'9']+
let string = '"' [^'"']* '"'
let ws = [' ' '\t']
let eol = ['\r' '\n']

rule main = parse
	| "Const"    as lxm {MODIFIER lxm}

	| identifier as lxm {IDENTIFIER lxm}
	| integer    as lxm {INTEGER lxm}
	| string     as lxm {STR (String.sub lxm 1 ((String.length lxm) - 2))}

	| "=" {EQUAL}

	| ws  {main lexbuf}
	| eol {EOL}
	| eof {EOF}

	| _ as c {CHAR c}
