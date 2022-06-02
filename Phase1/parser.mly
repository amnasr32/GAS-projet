%{
 (* code ocaml *)
 open Ast (* ou ast contient la syntax abstraite *)
 %}
 (* declaration des tokens (= terminaux ) *)


%token INPUTSYMBOLS STACKSYMBOLS STATES INITIALSTATE INITIALSTACKSYMBOL TRANSITIONS

%token VIRGULE PTVIRGULE RPAREN LPAREN EOF
%token <char> LETTRE

%start<Ast.automate> input

%%

input:
	c=automate EOF { c }

automate:
	d=declaration t=transitions					{ (d,t) }

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
	INITIALSTATE l=LETTRE							{ l }

initialstack:
	INITIALSTACKSYMBOL l=LETTRE							{ l }

suite_lettres_nonvide:
	| l=LETTRE									{ [l] }
	| l=LETTRE VIRGULE s=suite_lettres_nonvide	{ l::s }

transitions :

	TRANSITIONS
        t=translist						{ t }

translist:
	| 					       { [] }
	| t=transition
        r=translist					{ t::r }

transition:

    LPAREN
    	l1=LETTRE 			VIRGULE
    	l2=lettre_ou_vide 	VIRGULE
    	l3=LETTRE 			VIRGULE
    	l4=LETTRE 			VIRGULE
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
	| l=LETTRE PTVIRGULE s=nonemptystack		{ l::s }

