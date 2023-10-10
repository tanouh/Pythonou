open Ast

exception Error of string
exception RuntimeError of string * Lexing.position


let eval program_ast out =
  let rec eval_expr = function
  | Ecall(fct, [arg1]) -> call fct [eval_expr arg1]; 0
  | Const(Int s) -> int_of_string s
  | _ -> raise (Error "NAN ! ")
  and
    eval_stmt (stmt_node,pos) = match stmt_node with
  | Sval e -> let _ = eval_expr e in ()
  | Sblock b ->  let _ = List.iter eval_stmt b in ()
  | _ -> raise (RuntimeError ("Not implemented stmt !", pos))
and
   call fct args =
    let main_fct = List.hd (program_ast.defs) in
    assert (main_fct.name = "-#-main-#-");
    match fct with
    | "print" -> out (string_of_int (List.hd args))
    | "-#-main-#-" -> let _ = eval_stmt main_fct.body in ();
    | _ -> raise (Error "Function non implemented !")
in
  (* FILL ME *)
  let run (_program : Ast.prog) =
    (* FILL ME *)
    let _ = call "-#-main-#-" [] in
    ()
  in
  run program_ast
