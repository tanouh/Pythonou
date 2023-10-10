open Ast

exception Error of string
exception RuntimeError of string * Lexing.position

let eval program_ast out =
  let eval_expr (expr : Ast.expr) = expr in
  let eval_stmt (stmt : Ast.stmt) = stmt in
  let call fct args = match fct with "print" -> args in

  (* FILL ME *)
  let run (program : Ast.prog) =
    (* FILL ME *)
    let _ = call "-#-main-#-" [] in
    ()
  in
  run program_ast
