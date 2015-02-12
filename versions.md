* Affichage
* Variables + affichage
* Conditions
* Boucles

| _ as c {failwith ("unknown character: " ^ (String.make 1 c))}

(**
line:
     {}
  | line FUNC STR EOL {print_string $3; print_newline ()}
;
**)