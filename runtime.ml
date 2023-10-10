open Ast

exception Error of string
exception RuntimeError of string * Lexing.position

type ptype =
  | PInt of int
  | PBool of bool
  | PList of ptype list
  | PStr of string

module StrMap = Map.Make (String)



let gvars = Hashtbl.create 17

let _ = Hashtbl.add gvars "e" (PInt 5)

let string_of_ptype = function
  | PInt _ -> "<int>"
  | PBool _ -> "<bool>"
  | PList _ -> "<list>"
  | PStr _ -> "<string>"

let eval_binop_int a b = function
  | Add -> PInt (a + b)
  | Sub -> PInt (a - b)
  | Mul -> PInt (a * b)
  | Div -> (
      match b with 0 -> raise (Error "Divided By Zero") | b -> PInt (a / b))
  | Mod -> (
      match b with 0 -> raise (Error "Divided By Zero") | b -> PInt (a mod b))
  | Leq -> PBool (a <= b)
  | Le -> PBool (a < b)
  | Geq -> PBool (a >= b)
  | Ge -> PBool (a > b)
  | Neq -> PBool (a != b)
  | Eq -> PBool (a = b)
  | _ -> raise (Error "Unsupported operand for <int>")

let eval_binop_bool a b = function
  | Leq -> PBool (a <= b)
  | Le -> PBool (a < b)
  | Geq -> PBool (a >= b)
  | Ge -> PBool (a > b)
  | Neq -> PBool (a != b)
  | Eq -> PBool (a = b)
  | And -> PBool (a && b)
  | Or -> PBool (a || b)
  | _ -> raise (Error "Unsupported operand for <bool>")

let eval_binop_str a b = function
  | Add -> PStr (a ^ b)
  | Leq -> PBool (a <= b)
  | Le -> PBool (a < b)
  | Geq -> PBool (a >= b)
  | Ge -> PBool (a > b)
  | Neq -> PBool (a != b)
  | Eq -> PBool (a = b)
  | _ -> raise (Error "Unsupported operand for <string>")

let eval_binop_list a b = function
  | Add -> PList (List.rev_append (List.rev a) b)
  | Leq -> PBool (a <= b)
  | Le -> PBool (a < b)
  | Geq -> PBool (a >= b)
  | Ge -> PBool (a > b)
  | Neq -> PBool (a != b)
  | Eq -> PBool (a = b)
  | _ -> raise (Error "Unsupported operand for <list>")

let eval_binop a b binop =
  match (a, b) with
  | PInt a, PInt b -> eval_binop_int a b binop
  | PBool a, PBool b -> eval_binop_bool a b binop
  | PStr a, PStr b -> eval_binop_str a b binop
  | PList a, PList b -> eval_binop_list a b binop
  | a, b ->
      raise
        (Error
           ("Operation not allowed between " ^ string_of_ptype a ^ " and "
          ^ string_of_ptype b))

let rec print_ptype t =
  let string_of_list l =
    let rec loop l s =
      match l with
      | b :: [] -> s ^ print_ptype b
      | a :: l' -> loop l' (s ^ print_ptype a ^ ";")
      | [] -> ""
    in
    loop l "[" ^ "]"
  in
  match t with
  | PInt i -> string_of_int i
  | PBool b -> string_of_bool b
  | PList l -> string_of_list l
  | PStr s -> s

let build_const = function
  | Int k -> PInt (int_of_string k)
  | Str s -> PStr s
  | Bool b -> PBool b
  | Non -> raise (Error "Unknown value")

let assign locvars value = function
  | Tab (_x, _e) -> raise (Error "Not implemented !")
  | Var x -> StrMap.add x value locvars

let eval_var locvars = function
  | Var e ->
  if StrMap.mem e locvars then
  StrMap.find e locvars
  else if Hashtbl.mem gvars e then Hashtbl.find gvars e
  else raise (Error "Undefined variable\n")
  | _ -> raise (Error "not implemented")

let eval program_ast out =

  let rec eval_expr locvars = function
    | Ecall (fct, [ arg1 ]) ->
        call fct [ eval_expr locvars arg1 ];
        PInt 0
    | Const k -> build_const k
    | Op (binop, a, b) ->
        eval_binop (eval_expr locvars a) (eval_expr locvars b) binop
    | List l -> PList (List.map (eval_expr locvars) l)
    | Val v -> eval_var locvars v
    | _ -> raise (Error "not implemented !")

  and eval_stmt_block locvars = function
    | [] -> locvars
    | stmt :: b -> eval_stmt_block (eval_stmt locvars stmt) b

  and eval_loop locvars i stmt = function
    | PList l -> (
        match l with
        | v :: l' ->
            let locvars = eval_stmt (assign locvars v (Var i)) stmt in
            eval_loop locvars i stmt (PList l')
        | [] -> locvars)
    | _ -> raise (Error "The given type is not iterable")

  and eval_stmt locvars (stmt_node, pos) =
    match stmt_node with
    | Sval e ->
        let _ =
          try eval_expr locvars e
          with Error e -> raise (RuntimeError (e, pos))
        in
        locvars
    | Sblock b -> eval_stmt_block locvars b
    | Sassign (x, value) -> assign locvars (eval_expr locvars value) x
    | Sfor (i, expr, stmt) -> eval_loop locvars i stmt (eval_expr locvars expr)
    | _ -> raise (RuntimeError ("Not implemented stmt !", pos))
  and call fct args =
    let main_fct = List.hd program_ast.defs in
    assert (main_fct.name = "-#-main-#-");
    match fct with
    | "print" -> out (print_ptype (List.hd args))
    | "println" -> out (print_ptype (List.hd args) ^ "\n")
    | "-#-main-#-" ->
        let _ = eval_stmt StrMap.empty main_fct.body in
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
