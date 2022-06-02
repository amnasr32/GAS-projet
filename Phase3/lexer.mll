{
open Parser
}

let layout = [ ' ''\t''\n' ]
let lettre = [ 'a'-'z''A'-'Z''1'-'9' ]

rule main = parse
  | layout		    		{ main lexbuf }
  | "input symbols:"       	{ INPUTSYMBOLS }
  | "stack symbols:"       	{ STACKSYMBOLS }
  | "states:"              	{ STATES }
  | "initial state:"       	{ INITSTATE }
  | "initial stack:"      	{ INITSTACK }
  | "transitions:"         	{ TRANSITION }
  | ','			      	      	{ COMMA }
  | ';'			      		{ SEMICOLON }
  | '('			      		{ LPAREN }
  | ')'			      		{ RPAREN }
  | lettre as l				{ LETTRE l }
  | eof					      { EOF }
  | "program"         {PROGRAM}
  | "case state of"    {CASESTATE}
  | "case top of"      {CASETOP}
  | "case next of"     {CASENEXT}
  | "begin"            {BEGIN}
  | "push"             {PUSH}
  | ':'                {DP}
  | "pop"              {POP}
  | "change"           {CHANGE}
  | "reject"           {REJECT}
  | "end"              {ENDING} 
  | _					  	{ failwith "unexpected character" }

