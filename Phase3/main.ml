open Ast
open Printf

let lexbuf = Lexing.from_channel stdin

let ast = Parser.automate Lexer.main lexbuf

(*Affichage de l'automate *)
let _ = printf "%s\n" (as_string ast);;

