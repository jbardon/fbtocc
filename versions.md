* Affichage
* Variables + affichage
* Conditions
* Boucles

| _ as c {failwith ("unknown character: " ^ (String.make 1 c))}




main:
   line EOF {}
;

line:
	/* empty */ {}
	| line function EOL {print_newline ()}
;

function:
	| FUNC STR {print_string $1}
;
