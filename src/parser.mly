%{
	open Definitions
%}

%token EQUAL DEFTYPE CONSTMODIFIER DIMMODIFIER
%token <char> CHAR
%token <string> IDENTIFIER STR INTEGER VARTYPE COMMENT

%token EOL
%token EOF

%start main
%type <unit> main
%%

main:
   headers lines EOF footer {}
;

lines:
	/* empty */  {}
	| lines line {}	
;

line:
	| EOL {}
	| COMMENT { write ("//" ^ $1) }
	| function_call {}
	| var_def {}	
	| const_var_def {}
	| var_affect {}
;

function_call:
	| IDENTIFIER { dispatch_func $1 "" }
	| IDENTIFIER STR { dispatch_func $1 $2 }	
;

const_var_def:
	| CONSTMODIFIER IDENTIFIER EQUAL INTEGER { const_declaration (Int($2,$4)) }
	| CONSTMODIFIER IDENTIFIER EQUAL STR { const_declaration (Str($2,$4)) }	
;

var_def:
	| DIMMODIFIER IDENTIFIER DEFTYPE VARTYPE { var_declaration $4 $2 }
;

var_affect:
	| IDENTIFIER EQUAL INTEGER { var_affectation (Int($1,$3)) }
	| IDENTIFIER EQUAL STR { var_affectation (Str($1,$3)) }
;

headers:
	{ write_headers ()}
;

footer: 
	{ write_footer ()}
;
