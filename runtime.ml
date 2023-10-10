open Ast

exception Error of string
exception RuntimeError of string * Lexing.position

type ptype = Int of int | Bool of bool | List of ptype list | Str of string

let eval_binop_int a b = function
  | Add -> Int (a + b)
  | Sub -> Int (a - b)
  | Mul -> Int (a * b)
  | Div -> (
      match b with 0 -> raise (Error "Divided By Zero") | b -> Int (a / b))
  | Mod -> (
      match b with 0 -> raise (Error "Divided By Zero") | b -> Int (a mod b))
  | Leq -> Bool (a <= b)
  | Le -> Bool (a < b)
  | Geq -> Bool (a >= b)
  | Ge -> Bool (a > b)
  | Neq -> Bool (a != b)
  | Eq -> Bool (a = b)
  | _ -> raise (Error "Operation not allowed")

let eval_binop_list _a _b = function
  | _ -> raise (Error "Not implemented (list)")

let eval_binop_bool a b = function
  | And -> Bool (a && b)
  | Or -> Bool (a || b)
  | _ -> raise (Error "Operation not allowed")

let eval_binop_str _a _b = function
  | _ -> raise (Error "Not implemented !")

let eval_binop_list _a _b = function
  | _ -> raise (Error "Not implemented !")

let eval_binop a b binop =
  match (a, b) with
  | Int a, Int b -> eval_binop_int a b binop
  | Bool a, Bool b -> eval_binop_bool a b binop
  | _, _ -> raise (Error "not implemented !")

let string_of_list _l = ""

let print_ptype =
  function
  | Int i -> (string_of_int i)
  | Bool b -> (string_of_bool b)
  | List l -> (string_of_list l)
  | _ -> raise (Error "Not implemented !")

let eval program_ast out =
  let rec eval_expr = function
    | Ecall (fct, [ arg1 ]) ->
        call fct [ eval_expr arg1 ];
        Int(0)
    | Const (Int s) -> Int (int_of_string s)
    | Op (binop, a, b) -> eval_binop (eval_expr a) (eval_expr b) binop
    | _ -> raise (Error "NAN !")
  and eval_stmt (stmt_node, pos) =
    match stmt_node with
    | Sval e ->
        let _ = eval_expr e in
        ()
    | Sblock b ->
        let _ = List.iter eval_stmt b in
        ()
    | _ -> raise (RuntimeError ("Not implemented stmt !", pos))
  and call fct args =
    let main_fct = List.hd program_ast.defs in
    assert (main_fct.name = "-#-main-#-");
    match fct with
    | "print" -> out (print_ptype (List.hd args))
    | "-#-main-#-" ->
        let _ = eval_stmt main_fct.body in
        ()
    | _ -> raise (Error "Function not implemented !")
  in
  (* FILL ME *)
  let run (_program : Ast.prog) =
    (* FILL ME *)
    let _ = call "-#-main-#-" [] in
    ()
  in
  run program_ast
