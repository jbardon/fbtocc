open Parsing
open Error 

(** Write in output c file **)
let file = open_out "output.c"

let write line =
	output_string file line; 
	output_string file "\n"
;;  

let write_word word = 
	output_string file word
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

type t_var =
	| Int of string
	| String of string
	| Variable of string
	| Operation of t_var * string * t_var	

let table_variable :(string,string) Hashtbl.t = Hashtbl.create 5

let var_to_type name = 
	let primarytype = Hashtbl.find table_variable name in 
	match primarytype with
	| "Integer" -> Int(name)
	| "String" -> String(name)
	| _ -> failwith primarytype	
;;

let t_var_to_val = function
	| Int(value) | String(value) | Variable(value) -> value
	| _ -> failwith "not a value"
;;

let rec print_operation = function
	| Int(value) | String(value) | Variable(value) -> write_word (value) 
	| Operation(val1, op, val2) -> print_operation val1;
								   write_word (" " ^ op ^ " ");
								   print_operation val2
;;

let print_var_affectation name value = 
	write_word (name ^ " = ");
	print_operation value;
	write ";"
;;

let print_var_declaration name = 
	let variable = var_to_type name in 
	match variable with
	| Int _ -> write ("int " ^ name ^ ";") 
	| String _ -> write ("char* " ^ name ^ ";")
	| Variable(value) -> failwith value
	| _ -> failwith name
;;

let print_const_declaration name value primarytype = 
	match primarytype with
	| "Integer" -> write ("const int " ^ name ^ " = " ^ value ^ ";") 
	| "String" -> write ("char* " ^ name ^ " = " ^ value ^ ";")
	| _ -> failwith primarytype
;;

let write_args arg endv = match arg with 
	| Int(value) | String(value) | Variable(value) 
		-> write_word (value ^ (if endv then "" else ", ")) 
	| _ -> failwith "write_args"
;;

let rec print_args args = match args with
	| [] -> ()
	| hd :: tl -> if (List.length tl) = 0 then 
					write_args hd true
				  else write_args hd false;

				  print_args tl
;;

let print_func_call name args =
	write_word (name ^ "(");
	print_args args;
	write_word(");\n")
;;

type t =
	| Empty
	| Comment of string	
	| ConstDecl of string * string * string
	| VarDecl of string
	| VarAff of string * t_var
	| FuncCall of string * t_var list
	| IfState of string * t list * t list
	| ForState of t_var * string * string * t list
	| LoopState of string * t list

let convert_var = function
	| Int _ | String _  as a -> a
	| Variable(value) -> var_to_type value
	| _ -> failwith "convert_var"

let build_condition a comp b = 

	let ac = convert_var a in
	let bc = convert_var b in

	match (comp, ac, bc) with
	| (comp, Int v1, Int v2)
		-> (v1 ^ " " ^ comp ^ " " ^ v2)

	| (comp, String v1, String v2)
		-> ("strcmp(" ^ v1 ^ ", " ^ v2 ^ ") " ^ comp ^ " 0")

	| (comp, _, _)
		-> failwith comp	
;;

let var_declaration primarytype name =
	Hashtbl.add table_variable name primarytype
;; 	

let rec print_line = function
	| Empty -> ()
	| Comment(c) -> write c
	| ConstDecl(var, value, primarytype) -> print_const_declaration var value primarytype
	| VarDecl(v) -> print_var_declaration v
	| VarAff(var, value) -> print_var_affectation var value
	| FuncCall(name, args) -> print_func_call name args
	| IfState(cond, lines0, lines1) -> print_if_statement cond lines0 lines1
	| ForState(variable, min, max, lines) -> print_for_statement variable min max lines
	| LoopState(cond, lines) -> print_loop_statement cond lines

and print_evaluation lines = match lines with
	| [] -> ()
	| hd :: tl -> print_line hd;
				  print_evaluation tl

and print_if_statement cond lines0 lines1 = 
	write ("if(" ^ cond ^ "){");
	print_evaluation lines0;

	if (List.length lines1) > 0 then 
		write ("} else {");
		print_evaluation lines1;

	write "}"

and print_for_statement variable min max lines =

	let var_val = (t_var_to_val variable) in
	var_declaration	"Integer" var_val;
	print_var_declaration var_val;

	write_word "for (";
	write_word (var_val ^ " = " ^ min ^"; ");
	write_word (var_val ^ " < " ^ max ^"; ");
	write_word (var_val ^ "++) {\n");

	print_evaluation lines;
	write "}"

and print_loop_statement cond lines =
	write_word "while(";

	if cond = "" then
		write_word "true"
	else 
		write_word cond;

	write_word "){\n";
	print_evaluation lines;
	write "}"
;;

let print_code lines = 
	write_headers ();
	print_evaluation lines;
	write_footer ()
;;

(** Error override **)
let parse_error s = Error.error "Parsing error" (symbol_start_pos ())

(** Convert tokens to c language **)
let dispatch_func name args = 
	match name with
	| "Print" -> FuncCall("printf", args)
	| "Input" -> FuncCall("scanf", args)	
	| _ -> FuncCall(name, args)
;;
