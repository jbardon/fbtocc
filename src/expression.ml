type t =
	| Empty
	| Comment of string

let print_tree = List.iter (fun x -> print_endline x)	
