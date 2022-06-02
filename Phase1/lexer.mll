{
  (* prologue : les fonctions nécessaires dans les actions*)
  open Parser
}
(* déclaratin de la définition de jetons (terminaux)*)
let lettre = ['0'-'9''a'-'z''A'-'Z'] 
let layout = [ ' ''\t''\n' ]



(* déclaration de la liste des tokens
 *)
rule main = parse

  | '('			      	       	{ LPAREN }
  | ')'			      	       	{ RPAREN }
  | lettre as l			       	{ LETTRE l }
  | eof					            { EOF }
  | layout		    		      { main lexbuf }
  | "input symbols:"       	{ INPUTSYMBOLS }
  | "stack symbols:"       	{ STACKSYMBOLS }
  | "states:"              	{ STATES }
  | "initial state:"       	{ INITIALSTATE }
  | "initial stack symbol:"	{ INITIALSTACKSYMBOL }
  | "transitions:"         	{ TRANSITIONS }
  | ','			      	       	{ VIRGULE }
  | ';'			      	      	{ PTVIRGULE }
  | _					  	{ failwith "unexpected character" }