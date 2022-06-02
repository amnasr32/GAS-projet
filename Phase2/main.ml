open Ast

let lexbuf =Lexing.from_channel (stdin) 
let ast = Parser.input Lexer.main lexbuf 
(********* Pour qu'une automate soit déterministe il faut que 
1: pour chaque état q et pour chaque lettre α, il existe au plus
                une transition issue de q d’étiquette α

2: L’automate possède un et un seul état initial

      *)
(** les deux  fonctions qui vérifient la bonne syntaxe et affiche les erreurs 

*)
let verify (automate:automate) =  match automate with


										| ((input_symbols, stack_symbols, states, initial_state, initial_stack), transitions ) ->

																	(** Si  on a pas d'état initial dans les symboles d'état*)
											if not (List.exists (fun i -> i==initial_state) states) then 
											Printf.printf "\ninitial state not recognized\n"
											(*failwith "initial state not recognized"*);

   																(** Si  on a pas de symbol initial de pile *)

	                  	if not (List.exists (fun i -> i==initial_stack) stack_symbols) then 
											Printf.printf "\ninitial stack not recognized\n"

											(*failwith "initial stack not recognized"*);

let rec verify_deterministe (t:transitions) = match t with
			(** s'il n’y a aucune transition qui s’applique *)
			| [] -> Printf.printf "\nAutomate  deterministe\n"
			| (state, read_letter, stack_top, goto_state, replace_stack) :: r ->
							(** On vérifie s'il existe au plus une transition pour chaque état q et chaque lettre α issue de q d'étiquette α*)
				     if List.exists (fun (state2, read_letter2, stack_top2, goto_state2, replace_stack2) ->

					               state==state2 && stack_top==stack_top2 && (read_letter=read_letter2 ||
												 (** si les deux étiquettes ne sont pas égaux on vérifie s'il y'a epsilon*)
					             	(read_letter<>read_letter2 && (isNone(read_letter) || isNone(read_letter2)) ) ) ) r
					   then
						 Printf.printf "\nAutomate non deterministe\n"
						 
						 (*failwith "automate non deterministe"*)

				   	else
						 (** sinon  on continue le travail en récursif *)
						 verify_deterministe r

		in verify_deterministe transitions;;


		let _ = verify ast
		let _ = Printf.printf "\n****Affichage de l'automate:****\n\n%s\n" (as_string ast)