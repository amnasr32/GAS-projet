(******************* Syntaxe abstraite *****************)


(** sauf initialstack & initialstate qui sont des char le reste c'est uen liste de char*)
(** Les input sypbols sont une liste de char *)
type  symbols = char list ;;
(**transition  ( lettre , lettre-ou-vide , lettre , lettre , stack )*)
type transition = char * (char option) * char * char * symbols;;
(**ransitions → transitions: translist*)
type transitions = transition list;;
(**declarations → inputsymbols stacksymbols states initialstate initialstack*)
type declarations = symbols * symbols * symbols * char * char;;

(*automate → declarations transitions*)
type automate = declarations * transitions;;




                            (** fonction qui prend en paramètre une liste de char et un séparateur ;
                                  et la convertis en une chaine de caractère*)

let rec symbols_tostring (l:symbols) (del:string): string = match l with
  | [] -> ""
  | [c] -> Char.escaped c (**convertit le char en string*)
  | c::r -> (Char.escaped c) ^ del ^ " " ^ (symbols_tostring r del)



                            (** fonction qui prend une transition 
                                et qui la convertit en string*)


  let transition_tostring (t:transition) : string =  match t with
  (** cas  ou le deuxième element est un vide *)
  | (l1,None,l3,l4,stack ) -> Printf.sprintf "(%c, , %c, %c, %s)" l1 l3 l4 ((symbols_tostring stack ";" ))
  (** cas  ou le deuxième element est une lettre *)
  | (l1,Some(l2),l3,l4,stack ) -> Printf.sprintf "(%c, %c, %c, %c, %s)" l1 l2 l3 l4 ((symbols_tostring stack ";" ))
;;


                               (** fonction qui prend une liste de transitions 
                                et qui la convertit en string*)

let rec transitions_tostring (lst:transitions): string = match lst with
  | [] -> ""
  | (t:transition)::r -> (transition_tostring t) ^ "\n" ^ (transitions_tostring r)
;;



                          (** fonction qui permet de vérifier lettre ou non vide 
                                renvoie true s'il s'agit d'une lettre et false sinon 
                                ==> pour le moment ça ne m'a servi à rien ...
                          *)
let isNone v : bool = match v with
	| None -> true
	| _ -> false


                          (** fonction prends une declarations et qui la convertis en string
                          *)

let declarations_tostring (d:declarations) : string = match d with 
  | (s1,s2,s3,c4,c5) -> Printf.sprintf 	"input symbols: %s\nstack symbols: %s\nstates: %s\ninitial state: %s\ninitial stack symbol: %s\n\ntransitions:\n"

			(symbols_tostring s1 ",") (symbols_tostring s2 ",") (symbols_tostring s3 ",") (Char.escaped c4) (Char.escaped c5)

                          (** fonction qui prend en paramètre une autmate et qui la convertit en string*)
  let as_string (aut:automate) : string = match aut with
 	| (d, t) ->  Printf.sprintf "%s\n%s\n" (declarations_tostring d) (transitions_tostring t)

