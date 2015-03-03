%{
	(** Internal management **)	
	type t_variable = 
		| Int of string*string
		| Str of string*string 

	(** Write in output c file **)
	let file = open_out "output.c"

	let write line =
		output_string file "\t";
		output_string file line; 
		output_string file "\n"
  	;;  

	let write_headers () =
		output_string file "#include <stdio.h>\n";
		output_string file "#include <stdlib.h>\n";
		output_string file "\n";
		output_string file "int main(int argc, char** argv){\n"
	;;

	let write_footer () = 
		output_string file "\n\treturn EXIT_SUCCESS;\n";
		output_string file "}\n"
	;;

	(** Convert tokens to c language **)
	let printf arg = 
		write ("printf(\"" ^ arg ^ "\");")
	;;

	let dispatch_func name args = 
		match name with
		| "Print" -> printf args
		| _ -> failwith name
	;;

	let const_declaration =	function
		| Int(name, value) -> write ("const int " ^ name ^ " = " ^ value ^ ";")
		| Str(name, value) -> write ("char* " ^ name ^ " = \"" ^ value ^ "\";")
	;;

	let var_declaration primarytype name =
		match primarytype with
		| "Integer" -> write ("int " ^ name ^ ";")
		| "String" -> write ("char* " ^ name ^ ";")
		| _ -> failwith primarytype
	;;

	let var_affectation = function
		| Int(name, value) -> write (name ^ " = " ^ value ^ ";")
		| Str(name, value) -> write (name ^ " = \"" ^ value ^ "\";")
	;;

    let parse_error s = Error.error "Parsing error" (symbol_start_pos ())
%}

%token EQUAL DEFTYPE
%token <char> CHAR
%token <string> IDENTIFIER STR MODIFIER INTEGER VARTYPE

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
	| lines line {}
;

line:
	| EOL {}
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
	| MODIFIER IDENTIFIER EQUAL INTEGER { const_declaration (Int($2,$4)) }
	| MODIFIER IDENTIFIER EQUAL STR { const_declaration (Str($2,$4)) }	
;

var_def:
	| MODIFIER IDENTIFIER DEFTYPE VARTYPE { var_declaration $4 $2 }
;

var_affect:
	| IDENTIFIER EQUAL INTEGER { var_affectation (Int ($1,$3)) }
	| IDENTIFIER EQUAL STR { var_affectation (Str ($1,$3)) }
;

headers:
	{ write_headers ()}
;

footer: 
	{ write_footer ()}
;
