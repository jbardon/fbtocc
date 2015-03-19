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
   lines EOF { List.rev $1 }
;

lines:
	| /* empty */ { [Empty] }
	| lines line { $2::$1 }	
;

line:
	| EOL { Empty }
	| COMMENT { Comment ("//" ^ $1) }
	| const_var_def {$1}	
	| var_def {$1}
	| var_affect {$1}
	| function_call {$1}	
	| if_state {$1}
;

function_call:
	| IDENTIFIER STR { dispatch_func $1 $2 }	
;

var_def:
	| DIMMODIFIER IDENTIFIER DEFTYPE VARTYPE { var_declaration $4 $2; VarDecl($2) }
;

var_affect:
	| IDENTIFIER EQUAL INTEGER { VarAff($1,$3) }
	| IDENTIFIER EQUAL STR { VarAff($1,$3) }
;

const_var_def:
	| CONSTMODIFIER IDENTIFIER EQUAL INTEGER { ConstDecl($2,$4, "Integer") }
	| CONSTMODIFIER IDENTIFIER EQUAL STR { ConstDecl($2,$4, "String") }	
;

var_type:
	| INTEGER { Int($1) }
	| STR { String($1) }
	| IDENTIFIER { Variable($1) }
;

comparator:
	| EQUAL { "==" }
	| GTHAN { ">" }
	| GTHAN EQUAL { ">=" }
	| LTHAN { "<" }
	| LTHAN EQUAL { "<=" }
;

condition:
	| var_type comparator var_type { build_condition $1 $2 $3 }
;

if_state:
	| IFBEGIN condition IFTHEN lines IFEND { IfState($2,(List.rev $4)) }
;
