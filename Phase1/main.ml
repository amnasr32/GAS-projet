open Ast
(** fonction qui prend un état et une liste d'état et qui vérifie si l'état existe*)

let rec contains l e =
  match l with
  |e1::l1 -> if e1 = e then true else contains l1 e
  |[] -> false
  
(** fonction qui vérifie si l'état initial existe dans l'ensemble des états de l'automate
 elle vérifie si l'état initiaal de la pile existe dans la liste des stacksymbol*)

  let check_etat ast =
    match ast with
    |(a,b,c,d,e),f -> if (contains c d)=true then  
                            if ( contains b e) =true then 
                                  true 
                            else false 
                      else false
  
(** fonction qui mets à jour la pile:  update stack qui est une liste de char*)

  let rec update_stack old nw =
  match nw with
  |[] -> old
  |l::r -> update_stack (l::old) r

  (** fonction qui découpe un mot en liste de char *)
  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) [];;

  (** fonction qui récupère la première lettre d'un mot*)

  

	
			let get_first_lettre w =
				if String.length w = 0 then ' ' else String.get w 0 
  
  
  (** fonction qui enlève la première lettre d'un mot*)

  let remove_first_lettre w =
  if String.length w = 0 then "" else String.sub w 1 ((String.length w)-1)

    (** fonction qui prend un état ; l'état de la pile;

    les transitions et une lettre  qui est un char

    ==> elle cherche une la transition 
    *)

  let rec fetch_transition state stack (transitions: transitions ) lettre =

  match transitions with (**  le b est à remplacer avec some et none*)
   
 (**Ici b::r est une liste de char*)
  |(a,b,c,d,e)::ts -> (*si on est ds le même état*)
                          if state = a then
                                      (**on regarde s'il s'agit de la meme lettre*)


                                      match b with 
                                      |None ->                                        
                                        (** Si la lettre est epsilon*) 
                                          (* si on reste ds le meem etat on renvoie la transition*)
                                          if stack = c then (a,b,c,d,e)
                                          (** on passe à la transition suivante*)
                                          else  (fetch_transition state stack ts lettre)           

                                      |Some(x) ->  (*  on regardele symbol ini de la pile on reste ds le meme etat et on lit le b  *)

                                        if lettre=x then
                                                   
                                                    if stack = c then (a,b,c,d,e)
                                                    (** sinon on fait on récursif on regarde l'autre trans*)
                                                    else  fetch_transition state stack ts lettre
                                       
                                                                                                                  (*  on regardele symbol ini de la pile on reste ds le meme etat et on lit le b  *)
                                                    (*  on regardele symbol ini de la pile *)
                                                          
                                        (** s'il ne s'agit pas de la mêmle lettre*)
                                        else fetch_transition state stack ts lettre


                         (** s'il s'agit pas du même état*)               
                         else fetch_transition state stack ts lettre

  
	|[]-> if lettre = ' ' then failwith "pile non vide , entrée épuisée"  else failwith ("no transition found for : lettre : "^(String.make 1 lettre)^" state : "
	^(String.make 1 state)
	^" stack : "^(String.make 1 stack) )

	
	
	

  




  (** fonction qui lit un mot 
              prend un état; une liste de symble de pile ; des transitions et un mot                             
  *)
  and	 read_word (state:char) (stack : symbols) (transitions: transitions ) (word :string) =
  
  match (word,stack) with
  |"",[] -> true (* on a consommé le mot et la pile est vide *)
  |_,[] -> failwith "pile vide ,entrée non épuisée" (** pile vide sans qu'on ait consommé le mot*)
	(*On a consommé la totalité du mot mais la pile n'est pas vide *)
  |_,l3::stk -> print_char (get_first_lettre word); print_endline "\n" ;
	(let trans = (fetch_transition  state l3 transitions (get_first_lettre word)) in
														
                           match trans with
                           |(a,b,c,d,e) -> (*si vide = epsilon**)
                          		 match b with 
                            		|None -> read_word d (update_stack stk e) transitions word  
                            		|Some(x) -> read_word d (update_stack stk e) transitions (remove_first_lettre word))  
														(** si on trouve pas de transisions *)		
                         
  
  (** fonction qui execute un mot sur l'automate*)
   
  let run (ast: automate) word : bool=
  match ast with (* declaration,transition**)
  |(a,b,c,d,e),f -> read_word d [e] f word
     
  
    
  let lexbuffer file  = Lexing.from_channel (open_in file) 
  
  let ast lexbuf = Parser.input Lexer.main lexbuf 
  

  
  let _ =
    match Sys.argv with
    
    | [|_;"-run";filename;word|] -> if (run (ast (lexbuffer filename)) word) = true  then
    Printf.printf "Word accepted\n"
  else
    Printf.printf "Word not accepted\n"
    |_ -> Printf.printf "\noption indisponible !!"
    