open Parsing
open Error 

let table_variable :(string,string) Hashtbl.t = Hashtbl.create 5

type t =
	| Empty
	| Comment of string	
	| Variable of string

let print_line = function
	| Empty -> ()
	| Comment(c) -> print_string c
	| Variable(v) -> print_string ("var "^v^" type "^(Hashtbl.find table_variable v))

let rec print_evaluation lines = match lines with
	| [] -> ()
	| hd :: tl -> print_line hd;
				  print_evaluation tl

(** Internal management **)	
type t_variable = 
	| Int of string*string
	| Str of string*string 

(** Error override **)
let parse_error s = Error.error "Parsing error" (symbol_start_pos ())

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
	| "Integer" -> Hashtbl.add table_variable name primarytype; 
	| "String" -> write ("char* " ^ name ^ ";")
	| _ -> failwith primarytype
;;

let var_affectation = function
	| Int(name, value) -> write (name ^ " = " ^ value ^ ";")
	| Str(name, value) -> write (name ^ " = \"" ^ value ^ "\";")
;;

let if_statement cond = 
	write ("if(" ^ cond ^ "){");
	write ("}") 
;;
