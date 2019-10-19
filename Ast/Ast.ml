type pcl_type =
  | Typ_int
  | Typ_real
  | Typ_bool
  | Typ_char
  | Typ_array of int option * pcl_type
  | Typ_pointer of pcl_type


type id = string


type unop =
  | Uop_not
  | Uop_plus
  | Uop_minus


type binop =
  | Bop_plus
  | Bop_minus
  | Bop_times
  | Bop_rdiv
  | Bop_div
  | Bop_mod
  | Bop_or
  | Bop_and
  | Bop_eq
  | Bop_neq
  | Bop_less
  | Bop_leq
  | Bop_greater
  | Bop_geq


type ast = { prog_name : id; body : ast_body }


and ast_body = { decls : ast_local list; block : ast_block }


and ast_local =
  | Loc_var of (id list * pcl_type) list
  | Loc_label of id list
  | Loc_def of ast_header * ast_body
  | Loc_decl of ast_header


and ast_header =
  | H_proc of id * ast_formal list
  | H_func of id * ast_formal list * pcl_type


and ast_formal =
  | F_byval of id list * pcl_type
  | F_byref of id list * pcl_type


and ast_block = ast_stmt list


and ast_stmt =
  | St_empty
  | St_assign of ast_lvalue * ast_expr
  | St_block of ast_block
  | St_call of ast_call
  | St_if of ast_expr * ast_stmt * ast_stmt option
  | St_while of ast_expr * ast_stmt
  | St_label of id * ast_stmt
  | St_goto of id
  | St_return
  | St_new of ast_expr option * ast_lvalue
  | St_dispose of ast_lvalue


and ast_call = { routine_name : id; args : ast_expr list }


and ast_expr =
  | E_lvalue of ast_lvalue
  | E_rvalue of ast_rvalue


and ast_lvalue =
  | Lv_id of id
  | Lv_result
  | Lv_string of string
  | Lv_array of ast_lvalue * ast_expr
  | Lv_deref of ast_expr


and ast_rvalue =
  | Rv_int of int
  | Rv_bool of bool
  | Rv_real of float
  | Rv_char of char
  | Rv_nil
  | Rv_call of ast_call
  | Rv_ref of ast_lvalue
  | Rv_unop of unop * ast_expr
  | Rv_binop of ast_expr * binop * ast_expr


let make_tabs ~tabs = String.make tabs '\t'


let rec string_of_type pcl_type =
  match pcl_type with
  | Typ_int -> "int"
  | Typ_real -> "real"
  | Typ_bool -> "bool"
  | Typ_char -> "char"
  | Typ_array (None, pcl_type) -> Printf.sprintf "array [] of %s" (string_of_type pcl_type)
  | Typ_array (Some size, pcl_type) -> 
    Printf.sprintf "array [%s] of %s" (string_of_int size) (string_of_type pcl_type)
  | Typ_pointer pcl_type -> Printf.sprintf "pointer of %s" (string_of_type pcl_type)


let string_of_unop = function
  | Uop_not -> "not"
  | Uop_plus -> "+"
  | Uop_minus -> "-"


let string_of_binop = function
  | Bop_plus -> "+"
  | Bop_minus -> "-"
  | Bop_times -> "*"
  | Bop_rdiv -> "/"
  | Bop_div -> "div"
  | Bop_mod -> "mod"
  | Bop_or -> "or"
  | Bop_and -> "and"
  | Bop_eq -> "="
  | Bop_neq -> "<>"
  | Bop_less -> "<"
  | Bop_leq -> "=<"
  | Bop_greater -> ">"
  | Bop_geq -> ">="


let string_of_list to_string = function
  | [] -> ""
  | h::t -> List.fold_left (fun acc x -> Printf.sprintf "%s, %s" acc (to_string x)) (to_string h) t


let string_of_id_list = string_of_list Fun.id


let rec print_ast { prog_name; body } =
  Printf.printf "Program: %s\nBody:\n" prog_name;
  print_ast_body body ~tabs:1


and print_ast_body ~tabs { decls; block } =
  Printf.printf "%s[\n" (make_tabs ~tabs);
  List.iter (print_ast_local ~tabs:(tabs+1)) decls;
  Printf.printf "%s]\n" (make_tabs ~tabs);
  print_ast_block block ~tabs


and print_ast_local ~tabs = function
  | Loc_var vars ->
    List.iter
      (fun (ids, pcl_type) -> Printf.printf "%s%s : %s\n" (make_tabs ~tabs) (string_of_id_list ids) (string_of_type pcl_type))
      vars
  | Loc_label labels -> Printf.printf "%s%s : labels\n" (make_tabs ~tabs) (string_of_id_list labels)
  | Loc_def (header, body) -> print_ast_header header ~tabs; print_ast_body body ~tabs:(tabs+1)
  | Loc_decl header -> print_ast_header header ~tabs


and print_ast_block ~tabs ast_body =
  List.iter (print_ast_stmt ~tabs) ast_body


and print_ast_header ~tabs = function
  | H_proc (name, formals) ->
    Printf.printf "%sproc %s(%s)\n" (make_tabs ~tabs) name (string_of_ast_formals formals)
  | H_func (name, formals, pcl_type) ->
    Printf.printf "%sfunc %s(%s) : %s\n" (make_tabs ~tabs) name (string_of_ast_formals formals) (string_of_type pcl_type)


and string_of_ast_formals ast_formals =
  let to_string_element formal =
    let to_string_aux ids pcl_type =
      Printf.sprintf "%s : %s"
        (string_of_id_list ids) (string_of_type pcl_type)
    in
    match formal with
    | F_byval (ids, pcl_type) -> Printf.sprintf "val %s" (to_string_aux ids pcl_type)
    | F_byref (ids, pcl_type) -> Printf.sprintf "ref %s" (to_string_aux ids pcl_type)
  in
  string_of_list to_string_element ast_formals


and print_ast_stmt ~tabs = function
  | St_empty -> ()
  | St_assign (lvalue, expr) -> 
    Printf.printf "%s%s := %s\n" (make_tabs ~tabs) (string_of_ast_lvalue lvalue) (string_of_ast_expr expr)
  | St_block block -> print_ast_block block ~tabs
  | St_call call -> Printf.printf "%s%s\n" (make_tabs ~tabs) (string_of_ast_call call)
  | St_if (expr, stmt1, stmt2) -> 
    Printf.printf "%sif %s then\n" (make_tabs ~tabs) (string_of_ast_expr expr);
    print_ast_stmt stmt1 ~tabs:(tabs+1);
    if Option.is_some stmt2 then
    begin
      Printf.printf "%selse\n" (make_tabs ~tabs);
      print_ast_stmt (Option.get stmt2) ~tabs:(tabs+1)
    end
  | St_while (expr, stmt) ->
    Printf.printf "%swhile %s\n" (make_tabs ~tabs) (string_of_ast_expr expr);
    print_ast_stmt stmt ~tabs:(tabs+1)
  | St_label (label, stmt) ->
    Printf.printf "%s%s:\n" (make_tabs ~tabs) label;
    print_ast_stmt stmt ~tabs:(tabs+1)
  | St_goto id ->
    Printf.printf "%sgoto %s\n" (make_tabs ~tabs) id 
  | St_return ->
    Printf.printf "%sreturn from routine\n" (make_tabs ~tabs)
  | St_new (None, lvalue) ->
    Printf.printf "%snew %s\n" (make_tabs ~tabs) (string_of_ast_lvalue lvalue)
  | St_new (Some expr, lvalue) ->
    Printf.printf "%snew[%s] %s\n" (make_tabs ~tabs) (string_of_ast_expr expr) (string_of_ast_lvalue lvalue)
  | St_dispose lvalue ->
    Printf.printf "%sdispose %s\n" (make_tabs ~tabs) (string_of_ast_lvalue lvalue)


and string_of_ast_expr = function
  | E_lvalue lvalue -> string_of_ast_lvalue lvalue
  | E_rvalue rvalue -> string_of_ast_rvalue rvalue


and string_of_ast_lvalue = function
  | Lv_id id -> id
  | Lv_result -> "result"
  | Lv_string str -> Printf.sprintf "\"%s\"" str
  | Lv_array (lvalue, expr) -> Printf.sprintf "%s[%s]" (string_of_ast_lvalue lvalue) (string_of_ast_expr expr)
  | Lv_deref expr -> Printf.sprintf "^(%s)" (string_of_ast_expr expr)


and string_of_ast_rvalue = function
  | Rv_int inum -> string_of_int inum
  | Rv_bool bool -> string_of_bool bool
  | Rv_real fnum -> string_of_float fnum
  | Rv_char c -> Printf.sprintf "'%s'"(Char.escaped c)
  | Rv_nil -> "nil"
  | Rv_call call -> string_of_ast_call call
  | Rv_ref lvalue -> Printf.sprintf "@%s" (string_of_ast_lvalue lvalue)
  | Rv_unop (unop, expr) -> Printf.sprintf "%s(%s)" (string_of_unop unop) (string_of_ast_expr expr)
  | Rv_binop (expr1, binop, expr2) ->
    Printf.sprintf "(%s %s %s)" (string_of_ast_expr expr1) (string_of_binop binop) (string_of_ast_expr expr2)


and string_of_ast_call { routine_name; args } =
  Printf.sprintf "%s(%s)" routine_name (string_of_list string_of_ast_expr args)