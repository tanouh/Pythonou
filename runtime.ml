open Ast

type ptype =
  | PInt of int
  | PBool of bool
  | PList of ptype array
  | PStr of string
  | PVoid
  | PNone

exception Error of string
exception RuntimeError of string * Lexing.position
exception Return of ptype

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

let int_of_ptype t =
  match t with
  | PInt k -> k
  | PBool b -> if b then 1 else 0
  | PStr s -> int_of_char s.[0]
  | _ ->
      raise
        (Error
           ("The type " ^ string_of_ptype t ^ "cannot be converted as integer"))

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
      else loop (s ^ print_ptype l.(i) ^ ", ") (i + 1)
    in
    loop "[" 0 ^ "]"
  in
  match t with
  | PInt i -> string_of_int i
  | PBool b -> if b then "True" else "False"
  | PList l -> string_of_list l
  | PStr s -> s
  | _ -> raise (Error ("The type " ^ string_of_ptype t ^ " is not printable"))

let range t = match t with
  | PInt k -> PList (Array.init k (fun i -> PInt (i)))
  | PBool b -> if b then PList ([|PInt(0)|]) else PList ([||])
  | _ -> raise (Error ("The type " ^ string_of_ptype t ^ " cannot be interpreted as an integer"))


let build_const = function
  | Int k -> PInt (int_of_string k)
  | Str s -> PStr s
  | Bool b -> PBool b
  | Non -> raise (Error "Unknown value")

let eval_tab arr i =
  match (arr, i) with
  | PList arr, PInt i ->
      if Array.length arr < abs i then raise (Error "Indice out of bounds")
      else if i < 0 then arr.(Array.length arr + i)
      else arr.(i)
  | PList arr, PBool b ->
      if b then
        if Array.length arr = 1 then raise (Error "Indice out of bounds")
        else arr.(1)
      else arr.(2)
  | PStr s, PInt i ->
      if String.length s < abs i then raise (Error "Indice out of bounds")
      else if i < 0 then
        PStr (String.sub s (String.length s + i - 1) (String.length s + i))
      else PStr (String.sub s i (i + 1))
  | PStr s, PBool b ->
      if b then
        if String.length s = 1 then raise (Error "Indice out of bounds")
        else PStr (String.sub s 1 2)
      else PStr (String.sub s 0 1)
  | _, _ ->
      raise
        (Error ("The type " ^ string_of_ptype arr ^ " is not subscriptable"))


let eval program_ast out =
  let rec eval_expr locvars = function
    | Ecall (fct, args) -> (
        try
          let _ = call locvars fct args in
          PVoid
        with Return ret -> ret)
    | Const k -> build_const k
    | Op (binop, a, b) ->
        eval_binop (eval_expr locvars a) (eval_expr locvars b) binop
    | List l -> PList (Array.map (eval_expr locvars) (Array.of_list l))
    | Val v -> eval_val locvars v
    | _ -> raise (Error "not implemented !")
  and eval_val locvars = function
    | Var e ->
        if Hashtbl.mem locvars e then Hashtbl.find locvars e
        else if Hashtbl.mem gvars e then Hashtbl.find gvars e
        else raise (Error ("Undefined variable \"" ^ e ^ "\"\n"))
    | Tab (l, e) -> eval_tab (eval_val locvars l) (eval_expr locvars e)
  and eval_stmt_block locvars = function
    | [] -> ()
    | stmt :: b ->
        eval_stmt locvars stmt;
        eval_stmt_block locvars b
  and eval_loop locvars i stmt j t = match t with
    | PList l ->
        if j >= Array.length l then ()
        else (
          assign locvars l.(j) (Var i);
          eval_stmt locvars stmt;
          eval_loop locvars i stmt (j + 1) (PList l))
    | _ -> raise (Error ("The  type " ^ string_of_ptype t ^ " is not iterable"))
  and eval_if locvars expr body = (
    let v = (eval_expr locvars expr) in
    match v with
    | PBool b -> if b then (eval_stmt locvars body) else ()
    | _ -> raise (Error ("The type " ^ string_of_ptype v ^ " cannot be interpreted as a boolean"))
  )
  and eval_if_else locvars expr s e = (
    let v = (eval_expr locvars expr) in
    match v with
    | PBool b -> if b then (eval_stmt locvars s) else (eval_stmt locvars e)
    |_ -> raise (Error ("The type " ^ string_of_ptype v ^ " cannot be interpreted as a boolean"))
  )
  and eval_while locvars e s =
  let v = (eval_expr locvars e) in
    match v with
    | PBool b -> if b then
      ((eval_stmt locvars s);
      eval_while locvars e s)
      else ()
    |_ -> raise (Error ("The type " ^ string_of_ptype v ^ " cannot be interpreted as a boolean"))

  and eval_stmt locvars (stmt_node, pos) =
    match stmt_node with
    | Sval e ->
        let _ =
          try eval_expr locvars e
          with Error e -> raise (RuntimeError (e, pos))
        in
        ()
    | Sblock b -> eval_stmt_block locvars b
    | Sassign (x, value) -> assign locvars (eval_expr locvars value) x
    | Sfor (i, expr, stmt) ->
        eval_loop locvars i stmt 0 (eval_expr locvars expr)
    | Sif (expr, s) -> eval_if locvars expr s
    | Sifelse (expr, s, e) -> eval_if_else  locvars expr s e
    | Swhile(expr, s) -> eval_while locvars expr s
    | Sreturn expr -> raise (Return ((eval_expr locvars expr)))
    (* | _ -> raise (RuntimeError ("Not implemented stmt !", pos)) *)
  and call locvars fct args =
    let pargs = List.map (eval_expr locvars) args in
    match fct with
    | "print" -> out (print_ptype (List.hd pargs));
    | "println" -> out (print_ptype (List.hd pargs) ^ "\n");
    | "type" -> raise (Return (PStr (string_of_ptype (List.hd pargs))))
    | "len" -> raise (Return (PInt (len (List.hd pargs))))
    (* | "range" -> raise (Return (range (List.hd pargs))) *)
    | fct_name ->
        if Hashtbl.mem defs fct_name then
          let fct = Hashtbl.find defs fct_name in
          let fctvars =
            List.combine fct.args pargs |> List.to_seq |> Hashtbl.of_seq
          in
          let _ = eval_stmt fctvars fct.body in
          ()
        else raise (Error "Undefined function")
  and assign locvars value = function
    | Tab (x, e) -> (
      let rec loop = function
      | Tab(x,e) -> (match (loop x).(int_of_ptype (eval_expr locvars e)) with |PList arr -> arr | _ -> raise (Error "type"))
      | Var t -> match Hashtbl.find locvars t with |PList arr -> arr | _ -> raise (Error "type")
    in
      (loop x).(int_of_ptype (eval_expr locvars e)) <- value
    )
    | Var x -> Hashtbl.add locvars x value
  in

  (* FILL ME *)
  let run (_program : Ast.prog) =
    build_functions defs program_ast.defs;
    let _ = call (Hashtbl.create 5) "-#-main-#-" [] in
    ()
  in
  run program_ast
