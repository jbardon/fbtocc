%{
	let file = open_out "output.c"

	(** Write in output c file **)
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

	let const_declaration primarytype name value =	
		match primarytype with
		| "int" -> write ("const int " ^ name ^ " = " ^ value ^ ";")
		| "string" -> write ("char* " ^ name ^ " = \"" ^ value ^ "\";")
		| _ -> failwith primarytype
	;;
%}

%token EQUAL
%token <char> CHAR
%token <string> IDENTIFIER STR MODIFIER INTEGER

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
	| function_call {}
	| const_var_def {}
;

function_call:
	| IDENTIFIER { dispatch_func $1 "" }
	| IDENTIFIER STR { dispatch_func $1 $2 }	
;

const_var_def:
	| MODIFIER IDENTIFIER EQUAL INTEGER { const_declaration "int" $2 $4 }
	| MODIFIER IDENTIFIER EQUAL STR { const_declaration "string" $2 $4 }	
;

headers:
	{ write_headers ()}
;

footer: 
	{ write_footer ()}
;
