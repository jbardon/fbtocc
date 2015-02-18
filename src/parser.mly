%{
	let file = open_out "output.c"

	let write line =
		output_string file line; 
		output_string file "\n";
  	;;  

  	let write_headers () =
  		output_string file "#include <stdio.h>\n";
  		output_string file "#include <stdlib.h>\n";
  		output_string file "\n";
  		output_string file "int main(int argc, char** argv){\n";
  	;;

  	let write_footer () = 
  		output_string file "\treturn EXIT_SUCCESS;\n";
  		output_string file "}\n";
  	;;
%}

%token <char> CHAR
%token <string> FUNC STR

%token EOL
%token EOF

%start main
%type <unit> main
%%

main:
   headers lines EOF footer {}
;

lines:
	/* empty */ {}
	| lines line EOL {}
;

line:
	| FUNC {write $1}
	| FUNC STR {write $1}
;

headers:
	{ write_headers ()}
;

footer: 
	{ write_footer ()}
;
