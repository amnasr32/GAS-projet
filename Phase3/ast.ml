open Printf

type instr = Push of char | Pop | Reject | Change of char | CaseState of (operation list)
 				| CaseNext of (operation list) | CaseTop of (operation list)
and operation = char * instr

type symbols = char list;;
type transition = char * (char option) * char * char * symbols;;
type declarations = symbols * symbols * symbols * char * char;;
type input = Transitions of (transition list) | Program of instr
type automate = declarations * input;;

let rec list_toString l sep = match l with
  | [] -> ""
  | [v] -> Char.escaped v
  | v::r -> (Char.escaped v) ^ sep ^ " " ^ (list_toString r sep)
;;

let transition_toString t =  match t with
  | (v1,None,v3,v4,stack ) -> sprintf "(%c,  , %c, %c, %s)" v1 v3 v4 ((list_toString stack ";" ))
  | (v1,Some(v2),v3,v4,stack ) -> sprintf "(%c, %c, %c, %c, %s)" v1 v2 v3 v4 ((list_toString stack ";" ))
;;

let rec transitions_toString l = match l with
  | [] -> ""
  | t::r -> (transition_toString t) ^ "\n" ^ (transitions_toString r)
;;

let declaration_toString d = match d with
	| (v1, v2, v3, v4, v5) -> sprintf
		"input symbols: %s\nstack symbols: %s\nstates: %s\ninitial state: %s\ninitial stack symbol: %s\n"
			(list_toString v1 ",") (list_toString v2 ",") (list_toString v3 ",") (Char.escaped v4) (Char.escaped v5)
;;

let rec tab n=
	if( n == 0 ) then "" else "   " ^ (tab (n-1))
;;

let rec instr_toString ins t=
	let tabs = tab t in
    match ins with
    | Push c -> sprintf "push %c" c
    | Pop -> "pop"
    | Reject -> "reject"
    | Change n -> sprintf "change %c" n
    | CaseState l -> sprintf "begin\n%scase state of \n%s%send" (tab (t+1)) (operations_toString l (t+2)) tabs
    | CaseNext l -> sprintf "begin\n%scase next of \n%s%send" (tab (t+1)) (operations_toString l (t+2)) tabs
	| CaseTop l -> sprintf "begin\n%scase top of \n%s%send" (tab (t+1))  (operations_toString l (t+2)) tabs

and operations_toString ops t =
	let tabs = tab t in
	match ops with
	| [] -> ""
    | (c, CaseState([(c2,op)])) :: r -> sprintf "%s%c: begin case state of %c: %s end\n%s" tabs c c2 (instr_toString op 0) (operations_toString r t)
    | (c, CaseNext([(c2,op)])) :: r -> sprintf "%s%c: begin case next of %c: %s end\n%s" tabs c c2 (instr_toString op 0) (operations_toString r t)
    | (c, CaseTop([(c2,op)])) :: r -> sprintf "%s%c: begin case top of %c: %s end\n%s" tabs c c2 (instr_toString op 0) (operations_toString r t)
	| (c, ins) :: r -> sprintf "%s%c: %s\n%s" tabs c (instr_toString ins t) (operations_toString r t)
;;

let input_toString inp =
	match inp with
	| Transitions l -> transitions_toString l
	| Program p -> "Program: " ^ (instr_toString p 1)
;;

let as_string ast : string = match ast with
 	| (v1, v2) -> sprintf "%s\n%s\n" (declaration_toString v1) (input_toString v2)
;;

let isNone v =
	match v with
	| None -> true
	| _ -> false
;;
