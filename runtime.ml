open Ast ;;
 
exception  Error of string ;;
exception  RuntimeError of string * Lexing.position ;;
         
let eval program_ast (out: string -> unit) = 
  let eval_expr (expr:Ast.expr)=
  in
  let eval_stmt (stmt:Ast.stmt)=
  in
  let call _fct_name _args =
    (* FILL ME *)
    ()
  in
  let run (program:Ast.prog) =
  (* FILL ME *)
  let _ = call "-#-main-#-" [] in ()
  in run program_ast