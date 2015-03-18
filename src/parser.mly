%{
	open Definitions
%}

%token CONSTMODIFIER DIMMODIFIER
%token EQUAL GTHAN LTHAN DEFTYPE
%token IFBEGIN IFTHEN IFEND

%token <string> IDENTIFIER COMMENT
%token <string> STR INTEGER VARTYPE

%token EOL
%token EOF

%start main
%type <Definitions.t list> main
%%

main:
   lines EOF { $1 }
;

lines:
	| /* empty */ { [Empty] }
	| lines line { $2::$1 }	
;

line:
	| EOL { Empty }
	| COMMENT { Comment ("//" ^ $1) }
	| var_def { $1 }
/*
	| function_call {}	
	| const_var_def {}
	| var_affect {}
	| if_state {}
*/
;

var_def:
	| DIMMODIFIER IDENTIFIER DEFTYPE VARTYPE { var_declaration $4 $2; Variable($2) }
;

/*
function_call:
	| IDENTIFIER { dispatch_func $1 "" }
	| IDENTIFIER STR { dispatch_func $1 $2 }	
;

const_var_def:
	| CONSTMODIFIER IDENTIFIER EQUAL INTEGER { const_declaration (Int($2,$4)) }
	| CONSTMODIFIER IDENTIFIER EQUAL STR { const_declaration (Str($2,$4)) }	
;

var_affect:
	| IDENTIFIER EQUAL INTEGER { var_affectation (Int($1,$3)) }
	| IDENTIFIER EQUAL STR { var_affectation (Str($1,$3)) }
;

var_type:
	| INTEGER { $1 }
	| STR { "\"" ^ $1 ^ "\"" }
	| IDENTIFIER { $1 }
;

comparator:
	| EQUAL { "==" }
	| GTHAN { ">" }
	| GTHAN EQUAL { ">=" }
	| LTHAN { "<" }
	| LTHAN EQUAL { "<=" }
;

condition:
	| var_type comparator var_type { ($1 ^ " " ^ $2 ^ " " ^ $3) }
;

if_state:
	| IFBEGIN condition IFTHEN lines IFEND { if_statement $2 }
;

headers:
	{ write_headers () }
;

footer: 
	{ write_footer () }
;
*/