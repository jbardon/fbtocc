
%token <char> CHAR
%token EOF

%start main
%type <unit> main
%%

main:
   characters EOF {print_newline ()}
;

/*
 * A list of characters can be empty or composed of a list of characters
 * followed by a single character.
 *
 * This rule is left recursive because:
 *  - it is more efficient;
 *  - otherwise, the character input would be reversed. 
 */
characters:
    /* empty */ {}
  | characters CHAR {print_char $2}
;
