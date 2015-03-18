open Definitions

let () =
	if Array.length Sys.argv < 2
    	then print_endline "Please, give a file name" 
	else
  		let input_file = Sys.argv.(1) in
  		if not (Sys.file_exists input_file)
    		then print_endline "This file does not exist"
    	else
  			let lexbuf = Lexing.from_channel (open_in input_file) in
  			try
  				let f = Parser.main Scanner.main lexbuf in
          print_code f
  			with
  			 	Error.LexingError e -> Error.print e
