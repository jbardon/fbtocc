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
	output_string file "int main(int argc, char** argv){\n\n"
;;

let write_footer () = 
	output_string file "\n\treturn EXIT_SUCCESS;\n";
	output_string file "}\n"
;;

(** Variables register **)
type t_var =
	| Int of string
	| String of string
	| Variable of string
	| Operation of t_var * string * t_var	

let table_variable :(string,string) Hashtbl.t = Hashtbl.create 5

let var_declaration primarytype name =
	Hashtbl.add table_variable name primarytype
;; 	

let get_var name = 
	try
		let primarytype = Hashtbl.find table_variable name in 
		match primarytype with
		| "Integer" -> Int(name)
		| "String" -> String(name)
		| _ -> failwith primarytype	
	with 
		Not_found -> failwith ("Variable " ^ name ^ " not found in register")
;;

let get_var_notsafe name = 
	let primarytype = Hashtbl.find table_variable name in 
	match primarytype with
	| "Integer" -> Int(name)
	| "String" -> String(name)
	| _ -> failwith primarytype	
;;

let get_var_val = function
	| Int(value) | String(value) | Variable(value) -> value
	| _ -> failwith "not a value"
;;

let convert_var = function
	| Int _ | String _  as a -> a
	| Variable(value) -> get_var value
	| _ -> failwith "convert_var"

(** Print operations **)
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
	let variable = get_var name in 
	match variable with
	| Int _ -> write ("int " ^ name ^ ";") 
	| String _ -> write ("char* " ^ name ^ ";")
	| Variable(value) -> failwith value
	| _ -> failwith name
;;

let print_const_declaration name value =
 	let variable = get_var name in 
	match variable with
	| Int _ -> write ("const int " ^ name ^ " = " ^ value ^ ";") 
	| String _ -> write ("char* " ^ name ^ " = " ^ value ^ ";")
	| _ -> failwith name
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
	| ConstDecl of string * string
	| VarDecl of string
	| VarAff of string * t_var
	| FuncCall of string * t_var list
	| IfState of string * t list * t list
	| ForState of string * string * string * t list
	| LoopState of string * t list

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

let rec print_line = function
	| Empty -> ()
	| Comment(c) -> write c
	| ConstDecl(var, value) -> print_const_declaration var value
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

	try
		let aa = get_var_notsafe variable in
		print_string ""
	with Not_found ->
		var_declaration	"Integer" variable;
		print_var_declaration variable; 

	write_word "for (";
	write_word (variable ^ " = " ^ min ^"; ");
	write_word (variable ^ " < " ^ max ^"; ");
	write_word (variable ^ "++) {\n");

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
