{
  open Parser
}

let ws = [' ' '\t' '\r' '\n']

rule main = parse
	| eof {EOF}
	| _ as c {CHAR c}
