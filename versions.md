* Affichage
* Variables + affichage
* Conditions
* Boucles

| _ as c {failwith ("unknown character: " ^ (String.make 1 c))}


type t_variable = 
| Int of string*string
| Str of string*string

(Int ("a", "r"))

print = function
|Int(a,b) ->
ou match

type t_variable
| Bool of bool
| Int of int
| String of string
| Char of char

type t_prog = t_inst list 

type t_prog = 
| Empty
| Inst of t_inst * t_prog

type t_inst = 
| Print of string
| IF of t_rand * t_prog * t_prog
| IF of t_cond * t_inst list * t_inst list

if:
IF cond THEN inst ELSE inst { IF($2, $4, $6) }