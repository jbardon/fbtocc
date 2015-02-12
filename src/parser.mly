%{
	let file = open_out "output.c"

	let write line =
		output_string file line; 
		output_string file "\n";
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
   lines EOF {}
;

lines:
	/* empty */ {}
	| lines line EOL {}
;

line:
	| FUNC {write $1}
	| FUNC STR {write $1}
;
