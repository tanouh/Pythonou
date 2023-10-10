open Ast ;;
 
exception  Error of string ;;
exception  RuntimeError of string * Lexing.position ;;

let call _fct_name _args =
  (* FILL ME *)
  ()
           
let run (program:Ast.prog) =
  (* FILL ME *)
  let _ = call "-#-main-#-" [] in ()
