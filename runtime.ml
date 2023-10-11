open Ast

exception Error of string
exception RuntimeError of string * Lexing.position

type ptype =
  | PInt of int
  | PBool of bool
  | PList of ptype array
  | PStr of string
  | PVoid
  | PNone

module StrMap = Map.Make (String)

let gvars = Hashtbl.create 17
let defs = Hashtbl.create 17

(*let _ = Hashtbl.add gvars "e" (PInt 5)*)

let build_functions functions defs =
  List.iter (fun fct -> Hashtbl.add functions fct.name fct) defs

let len = function
  | PList l -> Array.length l
  | _ -> raise (Error "The given type has no length")

let string_of_ptype = function
  | PInt _ -> "<int>"
  | PBool _ -> "<bool>"
  | PList _ -> "<list>"
  | PStr _ -> "<string>"
  | PVoid -> "<void>"
  | PNone -> "<none>"

let rec plist_cmp a b =
  let cmp_size = compare (Array.length a) (Array.length b) in
  let rec loop i =
    if i >= Array.length a then if cmp_size = 0 then 0 else 1
    else if i >= Array.length b && cmp_size < 0 then -1
    else
      match (a.(i), b.(i)) with
      | PInt x, PInt y ->
          let v = Int.compare x y in
          if v < 0 then -1 else if v > 0 then 1 else loop (i + 1)
      | PBool x, PBool y ->
          let v = compare x y in
          if v < 0 then -1 else if v > 0 then 1 else loop (i + 1)
      | PList x, PList y ->
          let v = plist_cmp x y in
          if v < 0 then -1 else if v > 0 then 1 else loop (i + 1)
      | PStr x, PStr y ->
          let v = compare x y in
          if v < 0 then -1 else if v > 0 then 1 else loop (i + 1)
      | PVoid, PVoid ->
          let v = compare a.(i) b.(i) in
          if v != 0 then
            raise (Error "Unsupported operand for <none> and <none>")
          else loop (i + 1)
      | _, _ ->
          raise
            (Error
               (string_of_ptype a.(i)
               ^ " and "
               ^ string_of_ptype b.(i)
               ^ " are incomparable"))
  in
  loop 0

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

let eval_binop_list a b =
  print_int(plist_cmp a b);
  function
  | Add -> PList (Array.append a b)
  | Leq -> PBool (plist_cmp a b <= 0)
  | Le -> PBool (plist_cmp a b < 0)
  | Geq -> PBool (plist_cmp a b >= 0)
  | Ge -> PBool (plist_cmp a b > 0)
  | Neq -> PBool (plist_cmp a b != 0)
  | Eq -> PBool (plist_cmp a b = 0)
  | _ -> raise (Error "Unsupported operand for <list>")


let eval_binop_non a b binop =
  match (a, b) with
  | PNone, PNone -> (
      match binop with
      | Eq -> PBool true
      | Neq -> PBool false
      | _ -> raise (Error "Unsupported operand for <none> and <none>"))
  | _, _ -> (
      match binop with
      | Neq -> PBool true
      | Eq -> PBool false
      | _ ->
          raise
            (Error
               ("Unsupported operand for " ^ string_of_ptype a ^ " and "
              ^ string_of_ptype b)))

let eval_binop a b binop =
  match (a, b) with
  | PInt a, PInt b -> eval_binop_int a b binop
  | PBool a, PBool b -> eval_binop_bool a b binop
  | PStr a, PStr b -> eval_binop_str a b binop
  | PList a, PList b -> eval_binop_list a b binop
  | PNone, _ -> eval_binop_non a b binop
  | _, PNone -> eval_binop_non a b binop
  | _, _ ->
      raise
        (Error
           ("Operation not allowed between " ^ string_of_ptype a ^ " and "
          ^ string_of_ptype b))

let rec print_ptype t =
  let string_of_list l =
    let rec loop s i =
      if i >= Array.length l then s
      else if i = Array.length l - 1 then s ^ print_ptype l.(i)
      else loop (s ^ print_ptype l.(i) ^ ";") (i + 1)
    in
    loop "[" 0 ^ "]"
  in
  match t with
  | PInt i -> string_of_int i
  | PBool b -> string_of_bool b
  | PList l -> string_of_list l
  | PStr s -> s
  | _ -> raise (Error ("The type " ^ string_of_ptype t ^ " is not printable"))

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
      if StrMap.mem e locvars then StrMap.find e locvars
      else if Hashtbl.mem gvars e then Hashtbl.find gvars e
      else raise (Error "Undefined variable\n")
  | _ -> raise (Error "not implemented")

let eval program_ast out =
  let rec eval_expr locvars = function
    | Ecall (fct, l) -> call locvars fct l
    | Const k -> build_const k
    | Op (binop, a, b) ->
        eval_binop (eval_expr locvars a) (eval_expr locvars b) binop
    | List l -> PList (Array.map (eval_expr locvars) (Array.of_list l))
    | Val v -> eval_var locvars v
    | _ -> raise (Error "not implemented !")
  and eval_stmt_block locvars = function
    | [] -> locvars
    | stmt :: b -> eval_stmt_block (eval_stmt locvars stmt) b
  and eval_loop locvars i stmt j = function
    | PList l ->
        if j >= Array.length l then locvars
        else
          let locvars = eval_stmt (assign locvars l.(j) (Var i)) stmt in
          eval_loop locvars i stmt (j + 1) (PList l)
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
    | Sfor (i, expr, stmt) ->
        eval_loop locvars i stmt 0 (eval_expr locvars expr)
    (* | Sreturn e -> raise (RuntimeError ("Not implemented stmt !", pos)) *)
    | _ -> raise (RuntimeError ("Not implemented stmt !", pos))
  and call locvars fct args =
    let pargs = List.map (eval_expr locvars) args in
    match fct with
    | "print" ->
        out (print_ptype (List.hd pargs));
        PVoid
    | "println" ->
        out (print_ptype (List.hd pargs) ^ "\n");
        PVoid
    | "type" -> PStr (string_of_ptype (List.hd pargs))
    | "len" -> PInt (len (List.hd pargs))
    | fct ->
        if Hashtbl.mem defs fct then
          let _ = eval_stmt StrMap.empty (Hashtbl.find defs fct).body in
          PVoid
        else raise (Error ("The function " ^ fct ^ " is undefined"))
  in
  (* FILL ME *)
  let run (_program : Ast.prog) =
    build_functions defs program_ast.defs;
    let _ = call StrMap.empty "-#-main-#-" [] in
    ()
  in
  run program_ast
