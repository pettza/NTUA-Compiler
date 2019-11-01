open Ast
open Symtbl


exception TypingError of string


let ids_of_decls decls =
  let id_of_header = function
    | H_func (id, _)
    | H_proc (id, _) -> id
  in
  let ids_of_decl = function
    | Loc_var vars -> List.(concat @@ map fst vars) 
    | Loc_label labels -> labels
    | Loc_def (header, _)
    | Loc_decl header -> [id_of_header header]
  in
  List.(concat @@ map ids_of_decl decls)


let ids_of_formals formals =
  let ids_of_formal = function
    | F_byref (ids, _)
    | F_byval (ids, _) -> ids
  in
  List.(concat @@ map ids_of_formal formals)


let check_duplicate_ids ids =
  let module IdSet = Set.Make(Id) in
  let try_add s id =
    match IdSet.find_opt id s with
    | None -> IdSet.add id s
    | Some _ -> 
      let id = string_of_id id in
      raise @@ TypingError (Printf.sprintf "Duplicate definition of %s" id)
  in
  ignore @@ List.fold_left try_add IdSet.empty ids


let rec typecheck_ast { prog_name; body } =
  let symtbl = singleton prog_name Program in
  typecheck_ast_body ~symtbl body


and typecheck_ast_body ~symtbl { decls; block } =
  check_duplicate_ids @@ ids_of_decls decls;
  let symtbl' =
    List.fold_left (fun symtbl -> typecheck_add_ast_local ~symtbl) symtbl decls
  in
  typecheck_ast_block ~symtbl:symtbl' block


and typecheck_add_ast_local ~symtbl = function
  | Loc_var vars ->
    List.fold_left 
      (fun symtbl (ids, pcl_type) -> add_list ids (Variable pcl_type) symtbl)
      symtbl
      vars
  | Loc_label labels -> add_list labels Label symtbl
  | Loc_def (header, body) ->
    let symtbl' = typecheck_add_ast_header ~symtbl header in
    let get_formals = function
      | H_proc (_, formals)
      | H_func (_, (formals, _)) -> formals
    in
    let add_formal symtbl = function
      | F_byref (ids, pcl_type)
      | F_byval (ids, pcl_type) -> add_list ids (Variable pcl_type) symtbl
    in
    let symtbl'' =
      List.fold_left add_formal symtbl' @@ get_formals header 
    in
    let symtbl''' = 
      match header with
      | H_proc _ -> symtbl''
      | H_func (_, (_, pcl_type)) -> add (id_of_string "result") (Variable pcl_type) symtbl''
    in
    typecheck_ast_body ~symtbl:symtbl''' body;
    symtbl'
  | Loc_decl header -> typecheck_add_ast_header ~symtbl header


and typecheck_ast_block ~symtbl block = List.iter (typecheck_ast_stmt ~symtbl) block


and typecheck_add_ast_header ~symtbl = function
  | H_proc (id, formals) ->
    check_duplicate_ids @@ id :: ids_of_formals formals;
    add id (Procedure formals) symtbl
  | H_func (id, (formals, pcl_type)) ->
    check_duplicate_ids @@ id :: ids_of_formals formals;
    add id (Function (formals, pcl_type)) symtbl


and typecheck_ast_stmt stmt ~symtbl =
  let check_label id =
    try
      match find id symtbl with
      | Label -> ()
      | _ ->
        raise @@ TypingError (Printf.sprintf "%s is not a label" (string_of_id id))
    with Not_found ->
      raise @@ TypingError (Printf.sprintf "Undeclared label %s" (string_of_id id))
  in
  let check_bool expr =
    match type_of_ast_expr expr ~symtbl with
    | Typ_bool -> ()
    | _ ->
      raise @@ TypingError
        (Printf.sprintf "This expression is not boolean: %s"
        @@ Ast_print.string_of_ast_expr expr)
  in
  match stmt with
  | St_empty -> ()
  | St_assign (lvalue, expr) ->
    let l_type = type_of_ast_lvalue lvalue ~symtbl in
    begin
      match expr with
      | E_rvalue Rv_nil ->
        begin
          match l_type with
          | Typ_pointer _ -> ()
          | _ ->
            raise @@ TypingError
              (Printf.sprintf "%s cannot be assigned nil, it's not a pointer"
              @@ Ast_print.string_of_ast_lvalue lvalue)
        end
      | _ ->
        let e_type = type_of_ast_expr expr ~symtbl in
        if l_type = e_type then () 
        else
          raise @@ TypingError
            (Printf.sprintf "%s is assigned an expression of invalid type"
            @@ Ast_print.string_of_ast_lvalue lvalue)
    end
  | St_block block -> typecheck_ast_block block ~symtbl
  | St_call call -> ignore @@ type_of_ast_call call ~symtbl
  | St_if (expr, if_stmt, else_stmt) ->
    check_bool expr;
    typecheck_ast_stmt if_stmt ~symtbl;
    Option.iter (typecheck_ast_stmt ~symtbl) else_stmt
  | St_while (expr, stmt) ->
    check_bool expr;
    typecheck_ast_stmt stmt ~symtbl
  | St_label (id, stmt) ->
    check_label id;
    typecheck_ast_stmt stmt ~symtbl
  | St_goto id -> check_label id
  | St_return -> ()
  | St_new (expr, lvalue) ->
    let l_type = type_of_ast_lvalue lvalue ~symtbl in
    begin
      match Option.map (type_of_ast_expr ~symtbl) expr with
      | None ->
        begin
          match l_type with
          | Typ_pointer Typ_array (None, _) ->
            raise @@ TypingError
              (Printf.sprintf
              "%s is a pointer to an incomplete array and should be array-allocated"
              @@ Ast_print.string_of_ast_lvalue lvalue)
          | Typ_pointer _ -> ()
          | _ ->
            raise @@ TypingError
              (Printf.sprintf
              "Cannot allocate for %s, it is not a pointer"
              @@ Ast_print.string_of_ast_lvalue lvalue)
        end
      | Some Typ_int ->
        begin
          match l_type with
          | Typ_pointer Typ_array (None, _) -> ()
          | _ ->
            raise @@ TypingError
              (Printf.sprintf
              "Cannot array-allocate %s, it is not a pointer to an incomplete array"
              @@ Ast_print.string_of_ast_lvalue lvalue)
        end
      | _ ->
        raise @@ TypingError
        (Printf.sprintf "%s is not integer. Cannot allocate non-integer memory"
        @@ Ast_print.string_of_ast_expr @@ Option.get expr)
    end
  | St_dispose (paren_opt, lvalue) ->
    let l_type = type_of_ast_lvalue lvalue ~symtbl in
      begin
        match paren_opt with
        | None ->
          begin
            match l_type with
            | Typ_pointer Typ_array (None, _) ->
              raise @@ TypingError
                (Printf.sprintf
                "%s is a pointer to an incomplete array and should be array-deallocated"
                @@ Ast_print.string_of_ast_lvalue lvalue)
            | Typ_pointer _ -> ()
            | _ ->
              raise @@ TypingError
                (Printf.sprintf
                "Cannot deallocate %s, it is not a pointer"
                @@ Ast_print.string_of_ast_lvalue lvalue)
          end
        | Some () ->
          match l_type with
          | Typ_pointer Typ_array (None, _) -> ()
          | _ ->
            raise @@ TypingError
              (Printf.sprintf
              "Cannot array-deallocate %s, it is not a pointer to an incomplete array"
              @@ Ast_print.string_of_ast_lvalue lvalue)

      end


and type_of_ast_lvalue ~symtbl = function
  | Lv_id id ->
    begin
      try
        match find id symtbl with
        | Variable pcl_type -> pcl_type
        | _ ->
          raise @@ TypingError (Printf.sprintf "%s is not a valid lvalue" (string_of_id id)) 
      with Not_found ->
        raise @@ TypingError (Printf.sprintf "Undeclared variable %s" (string_of_id id)) 
    end
  | Lv_result ->
    begin
      try
        let res_id = id_of_string "result" in
        match find res_id symtbl with
        | Variable pcl_type -> pcl_type
        | _ ->
          (* Should be unreachable *)
          failwith "This code should not be reachable. In function type_of_ast_lvalue"
      with Not_found ->
        raise @@ TypingError "Keyword result cannot be used here" 
    end
  | Lv_string _ -> Typ_array (None, Typ_char)
  | Lv_array (lvalue, expr) ->
    let () =
      match type_of_ast_expr expr ~symtbl with
      | Typ_int -> ()
      | _ ->
        raise @@ TypingError "Index expression should be integer"
    in
    begin
      match type_of_ast_lvalue lvalue ~symtbl with
      | Typ_array (_, pcl_type) -> pcl_type
      | _ ->
        raise @@ TypingError
        (Printf.sprintf "%s is not an array and it cannot be indexed"
        @@ Ast_print.string_of_ast_lvalue lvalue)
    end
  | Lv_deref expr ->
    begin
      match type_of_ast_expr expr ~symtbl with
      | Typ_pointer pcl_type -> pcl_type
      | _ ->
        raise @@ TypingError
        (Printf.sprintf "%s is not a pointer and cannot be dereferenced"
        @@ Ast_print.string_of_ast_expr expr)      
    end


and type_of_ast_rvalue ~symtbl = function
  | Rv_int _ -> Typ_int
  | Rv_bool _ -> Typ_bool
  | Rv_real _ -> Typ_real
  | Rv_char _ -> Typ_char
  | Rv_nil ->
    (* Should be unreachable *)
    failwith "This code should not be reachable. In function type_of_ast_rvalue"
  | Rv_call call ->
    begin
      match type_of_ast_call call ~symtbl with
      | Some pcl_type -> pcl_type
      | None -> 
        let { routine_name=id; _ } = call in
        raise @@ TypingError 
          (Printf.sprintf "Procedure %s cannot be called in expression"
          @@ string_of_id id)
    end
  | Rv_ref lvalue -> Typ_pointer (type_of_ast_lvalue lvalue ~symtbl)
  | Rv_unop (unop, expr) ->
    let e_type = type_of_ast_expr expr ~symtbl in 
    typecheck_unop unop e_type;
    e_type
  | Rv_binop (expr1, binop, expr2) ->
    let e1_type = type_of_ast_expr expr1 ~symtbl in
    let e2_type = type_of_ast_expr expr2 ~symtbl in
    type_of_binop binop e1_type e2_type


and type_of_ast_expr ~symtbl = function
  | E_lvalue lvalue -> type_of_ast_lvalue lvalue ~symtbl
  | E_rvalue rvalue -> type_of_ast_rvalue rvalue ~symtbl


and typecheck_unop unop e_type =
  match unop with
  | Uop_not ->
    if e_type <> Typ_bool then
      raise @@ TypingError "Logical nagation applied to non-boolean expession"
    else ()
  | Uop_plus | Uop_minus as unop ->
    if e_type <> Typ_int || e_type <> Typ_real then
      let s = Ast_print.string_of_unop unop in
      raise @@ TypingError (Printf.sprintf "Unary %s applied to non-arithmetic expession" s)
    else ()


and type_of_binop binop e1_type e2_type =
  let is_int = function
    | Typ_int -> true
    | _ -> false
  in
  let is_real = function
    | Typ_real -> true
    | _ -> false
  in
  let is_bool = function
    | Typ_bool -> true
    | _ -> false
  in
  match binop with
  | Bop_plus | Bop_minus | Bop_times ->
    if (is_int e1_type || is_real e1_type) && e1_type = e2_type
    then e1_type
    else raise @@ TypingError "Type mismatch"
  | Bop_rdiv ->
    if is_real e1_type && is_real e2_type
    then e1_type
    else raise @@ TypingError "Type mismatch"
  | Bop_div
  | Bop_mod ->
    if is_int e1_type && is_int e2_type
    then e1_type
    else raise @@ TypingError "Type mismatch"
  | Bop_or
  | Bop_and ->
    if is_bool e1_type && is_bool e2_type
    then e1_type
    else raise @@ TypingError "Type mismatch"
  | Bop_eq | Bop_neq ->
    let () =
      match e1_type with
      | Typ_bool | Typ_char | Typ_int | Typ_real | Typ_pointer _ -> ()
      | Typ_array _ ->
        raise @@ TypingError
        (Printf.sprintf "Cannot check equality of %s"
        @@ Ast_print.string_of_type e1_type)
    in
    if e1_type = e2_type
    then Typ_bool
    else raise @@ TypingError "Type mismatch"
  | Bop_less | Bop_leq | Bop_greater | Bop_geq ->
    if (is_int e1_type || is_real e1_type) && e1_type = e2_type
    then Typ_bool
    else raise @@ TypingError "Type mismatch"


and type_of_ast_call { routine_name; args } ~symtbl =
  let check_args formals =
    let types_of_formals =
      List.(concat @@ map
        (function | F_byref (ids, pcl_type) | F_byval (ids, pcl_type) ->
          map (fun _ -> pcl_type) ids)
        formals) 
    in
    try
      if List.(for_all ((=) true) @@ map2 (=) types_of_formals @@ map (type_of_ast_expr ~symtbl) args)
      then ()
      else
        raise @@ TypingError "Type mismatch"
    with Invalid_argument _ ->
      raise @@ TypingError 
        (Printf.sprintf "Wrong number of arguments in call to %s"
        @@ string_of_id routine_name)
  in
  try
    match find routine_name symtbl with
    | Function (formals, pcl_type) ->
      check_args formals;
      Some pcl_type
    | Procedure formals ->
      check_args formals;
      None
    | _ ->
      raise @@ TypingError
        (Printf.sprintf "%s is not callable" @@ string_of_id routine_name)
  with Not_found ->
    raise @@ TypingError
      (Printf.sprintf "Undeclared routine %s" @@ string_of_id routine_name)