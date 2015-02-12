
%token <char> CHAR
%token <string> FUNC STR

%token EOL
%token EOF


%start main
%type <unit> main
%%

main:
   line EOF {}
;

line:
	/* empty */ {}
	| function EOL {print_newline ()}
;

function:
	| FUNC STR {print_string $1}
;
