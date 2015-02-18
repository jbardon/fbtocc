* Affichage
* Variables + affichage
* Conditions
* Boucles

| _ as c {failwith ("unknown character: " ^ (String.make 1 c))}



footer: 
	{ write_footer }
;

  	let write_footer = 
  		output_string file "\treturn EXIT_SUCCESS;\n";
  		output_string file "}\n";
  	;;
