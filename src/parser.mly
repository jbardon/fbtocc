%{
	open Definitions
%}

%token CONST DIM
%token EQUAL GTHAN LTHAN AS COMMA
%token IF THEN ELSE IFEND FOR TO NEXT WHILE DO LOOP

%token <string> IDENTIFIER COMMENT
%token <string> STR INTEGER VARTYPE OPERATION

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
	| for_state {$1}
	| loop {$1}
;

function_call:
	| IDENTIFIER args { dispatch_func $1 $2 }	
;

args:
	| {[]}
	| args_list { List.rev $1 }
;

args_list:
	| var_type {[$1]}	
	| args_list COMMA var_type {$3::$1}
;

var_def:
	| DIM IDENTIFIER AS VARTYPE { var_declaration $4 $2; VarDecl($2) }
;

var_affect:
	| IDENTIFIER EQUAL expr { VarAff($1,$3) }
	| IDENTIFIER EQUAL expr { VarAff($1,$3) }
;

expr: 
	| var_type {$1}
	| expr OPERATION expr { Operation($1,$2,$3) }
;

const_var_def:
	| CONST IDENTIFIER EQUAL INTEGER { ConstDecl($2,$4, "Integer") }
	| CONST IDENTIFIER EQUAL STR { ConstDecl($2,$4, "String") }	
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
	| IF condition THEN lines IFEND { IfState($2,(List.rev $4),[]) }
	| IF condition THEN lines ELSE lines IFEND { IfState($2,(List.rev $4),(List.rev $6)) }
;

for_state:
	| FOR IDENTIFIER EQUAL INTEGER TO INTEGER lines NEXT { ForState(Variable($2),$4,$6,$7) }
;

loop:
	| DO WHILE condition lines LOOP { LoopState($3,$4) }
	| DO lines LOOP { LoopState("",$2) }
;
