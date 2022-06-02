%{
open Ast
%}

%token INPUTSYMBOLS STACKSYMBOLS STATES INITSTATE INITSTACK TRANSITION
%token COMMA SEMICOLON RPAREN LPAREN EOF 
%token PUSH POP REJECT PROGRAM CASESTATE BEGIN ENDING DP CASENEXT CASETOP CHANGE
%token <char> LETTRE

%start<Ast.automate> automate

%%

automate:
	| d=declaration t=input	EOF			{ (d,t) }

declaration:
	symbols=inputsymbols stacks=stacksymbols st=states init_state=initialstate init_stack=initialstack
				{ ( symbols, stacks, st, init_state, init_stack) }

inputsymbols:
	INPUTSYMBOLS s=suite_lettres_nonvide		{ s }

stacksymbols:
	STACKSYMBOLS s=suite_lettres_nonvide		{ s }

states:
	STATES s=suite_lettres_nonvide				{ s }

initialstate:
	INITSTATE l=LETTRE							{ l }

initialstack:
	INITSTACK l=LETTRE							{ l }

suite_lettres_nonvide:
	| l=LETTRE									{ [l] }
	| l=LETTRE COMMA s=suite_lettres_nonvide	{ l::s }

input:
	| TRANSITION t=translist					{ Transitions t }
	| PROGRAM DP s=case 						{ Program s }
translist:
	| t=transition 								{ [t] }
	| t=transition r=translist					{ t::r }

transition:
    LPAREN
    	l1=LETTRE 			COMMA
    	l2=lettre_ou_vide 	COMMA
    	l3=LETTRE 			COMMA
    	l4=LETTRE 			COMMA
    	s=stack
    RPAREN                						{ (l1,l2,l3,l4,s) }

lettre_ou_vide:
	| 											{ None }
	| l=LETTRE									{ Some l }

stack:
	| 											{ [] }
	|	s=nonemptystack							{ s }

nonemptystack:
	| l=LETTRE									{ [l] }
	| l=LETTRE SEMICOLON s=nonemptystack		{ l::s }



case:
	| CASETOP   s=suite_instr_non_vide			{ CaseTop(s) }
	| CASENEXT  s=suite_instr_non_vide			{ CaseNext(s) }
	| CASESTATE s=suite_instr_non_vide			{ CaseState(s) }

suite_instr_non_vide:
   | l=LETTRE DP i=instr						{ [(l, i)] }
   | l=LETTRE DP i=instr r=suite_instr_non_vide { (l, i) :: r }

instr:
	| REJECT 									{ failwith "reject" }
	| PUSH l=LETTRE								{ Push(l) }
	| CHANGE l=LETTRE							{ Change(l) }
	| POP										{ Pop }
	| BEGIN c=case ENDING						{ c }
