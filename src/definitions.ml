open Parsing
open Error 

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

let table_variable :(string,string) Hashtbl.t = Hashtbl.create 5

let print_var_affectation name value = 
	let primarytype = Hashtbl.find table_variable name in 
	match primarytype with
	| "Integer" -> write (name ^ " = " ^ value ^ ";") 
	| "String" -> write (name ^ " = " ^ value ^ ";")
	| _ -> failwith primarytype
;;

let print_var_declaration name = 
	let primarytype = Hashtbl.find table_variable name in 
	match primarytype with
	| "Integer" -> write ("int " ^ name ^ ";") 
	| "String" -> write ("char* " ^ name ^ ";")
	| _ -> failwith primarytype
;;

let print_const_declaration name value primarytype = 
	match primarytype with
	| "Integer" -> write ("const int " ^ name ^ " = " ^ value ^ ";") 
	| "String" -> write ("char* " ^ name ^ " = " ^ value ^ ";")
	| _ -> failwith primarytype
;;

let print_func_call name args =
	write (name ^ "(" ^ args ^ ");")
;;

type t =
	| Empty
	| Comment of string	
	| ConstDecl of string * string * string
	| VarDecl of string
	| VarAff of string * string
	| FuncCall of string * string
	| IfState of string * t list

let print_line = function
	| Empty -> ()
	| Comment(c) -> write c
	| ConstDecl(var, value, primarytype) -> print_const_declaration var value primarytype
	| VarDecl(v) -> print_var_declaration v
	| VarAff(var, value) -> print_var_affectation var value
	| FuncCall(name, args) -> print_func_call name args
	| IfState(cond, lines) -> print_if_statement cond lines

let rec print_evaluation lines = match lines with
	| [] -> ()
	| hd :: tl -> print_line hd;
				  print_evaluation tl

let print_code lines = 
	write_headers ();
	print_evaluation lines;
	write_footer ()
;;

let print_if_statement cond lines = 
	write ("if(" ^ cond ^ "){");
	print_evaluation lines;
	write "}"
;;

(** Error override **)
let parse_error s = Error.error "Parsing error" (symbol_start_pos ())

(** Convert tokens to c language **)
let dispatch_func name args = 
	match name with
	| "Print" -> FuncCall("printf", args)
	| _ -> failwith name
;;

let var_declaration primarytype name =
	Hashtbl.add table_variable name primarytype
;; 	
