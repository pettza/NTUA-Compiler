open Ast
open Symtbl


exception TypingError of string


(* Checks if the id already exists in the symbol table, if it doesn't,
it adds it, otherwise it updates the entry only in the case that the
previous entry was the forward declaration of a routine and the one
that replaces it is its implementation *)
let update_entry id sym_entry symtbl = 
  let check_duplicate sym_entry = function
    | None -> Some sym_entry
    | Some Function (func_type1, { def=false }) ->
      begin
        match sym_entry with
        | Function (func_type2, { def=true }) when func_type1 = func_type2 -> Some sym_entry
        | Function (_, _) -> 
          raise @@ TypingError
            (Printf.sprintf "function %s declared more than once in this group of local declerations" id)
        | _ ->
          raise @@ TypingError
            (Printf.sprintf "duplicate id %s" id)
      end
    | Some Procedure (proc_type1, { def=false }) ->
      begin
        match sym_entry with
        | Procedure (proc_type2, { def=true }) when proc_type1 = proc_type2 -> Some sym_entry
        | Procedure (_, _) -> 
          raise @@ TypingError
            (Printf.sprintf "procedure %s declared more than once in this group of local declerations" id)
        | _ ->
          raise @@ TypingError
            (Printf.sprintf "duplicate id %s" id)
      end
    | _ ->
      raise @@ TypingError
        (Printf.sprintf "duplicate id %s" id)
  in
    update id (check_duplicate sym_entry) symtbl


(* Returns a list with the ids used in the formal arguments *)
let ids_of_formals formals =
  let id_of_formal = function
    | F_byref (id, _)
    | F_byval (id, _) -> id
  in
  List.map id_of_formal formals


(* Checks whether an id appears more then once in a list
and raises an error if there is such id *)
let check_duplicate_ids ids =
  let module IdSet = Set.Make(Id) in
  let try_add s id =
    match IdSet.find_opt id s with
    | None -> IdSet.add id s
    | Some _ ->
      raise @@ TypingError (Printf.sprintf "Duplicate definition of %s" id)
  in
  ignore @@ List.fold_left try_add IdSet.empty ids


(* Checks whether a value of rhs_type can be assigned to a value of lhs type *)
let is_assignable lhs_type rhs_type =
  match lhs_type, rhs_type with
  | Typ_real, Typ_int -> true
  | Typ_pointer (Some _), Typ_pointer None
  | Typ_pointer None, Typ_pointer (Some _) -> true
  | Typ_pointer (Some Typ_array (None, pcl_type1)), Typ_pointer (Some Typ_array (Some _, pcl_type2)) ->
    pcl_type1 = pcl_type2
  | Typ_pointer (Some Typ_array (None, _)), pcl_type ->
    raise @@ TypingError
     (Printf.sprintf "Value of type %s cannot be assigned to incomplete array"
     @@ Ast_print.string_of_type pcl_type)
  | pcl_type1, pcl_type2 -> pcl_type1 = pcl_type2


(* Checks whether an a value of rhs_type can be passed by reference
to a formal argument of type lhs_type *)
let is_referencable lhs_type rhs_type =
  is_assignable (Typ_pointer (Some lhs_type)) (Typ_pointer (Some rhs_type))


(* Symbol table containing the library routines *)
let library_symtbl =
  empty
  |> add "writeInteger" @@ Procedure ([F_byval ("i", Typ_int)], { def=true })


(* Typechecks the abstact syntax tree *)
let rec typecheck_ast { prog_name=_; body } =
  typecheck_ast_body ~symtbl:library_symtbl body


(* Typechecks the body of program and routines.
Raises an exception in case of error *)
and typecheck_ast_body ~symtbl { decls; block } =
  let symtbl' =
    List.fold_left (fun symtbl_loc -> typecheck_add_ast_local ~symtbl ~symtbl_loc) empty decls
  in
  let check_used_labels () =
    decls
    |> List.filter_map (function Loc_label l -> Some l | _ -> None)
    |> List.concat
    |> List.map (fun id -> (id, find id symtbl'))
    |> List.find_opt (function (_, Label { used=false }) -> true | _ -> false)
    |> Option.map (fun (id, _) -> raise @@ TypingError (Printf.sprintf "Unused label %s" id))
  in
  typecheck_ast_block ~symtbl:(add_tbl symtbl symtbl') block;
  ignore @@ check_used_labels ()



(* Typechecks local declarations and returns an updated symbol table.
symtlb_loc is needed to detect duplicate definitions whilst ignoring
name shadowing *)
and typecheck_add_ast_local ~symtbl ~symtbl_loc = function
  | Loc_var vars ->
    List.fold_left 
      (fun symtbl (id, pcl_type) -> update_entry id (Variable pcl_type) symtbl)
      symtbl_loc
      vars
  | Loc_label labels ->
    List.fold_left 
      (fun symtbl label -> update_entry label (Label { used=false }) symtbl)
      symtbl_loc
      labels
  | Loc_def (header, body) ->
    let symtbl' = typecheck_add_ast_header true ~symtbl_loc header in
    let get_formals = function
      | H_proc (_, formals)
      | H_func (_, (formals, _)) -> formals
    in
    let add_formal symtbl = function
      | F_byref (id, pcl_type)
      | F_byval (id, pcl_type) -> add id (Variable pcl_type) symtbl
    in
    let symtbl'' =
      List.fold_left add_formal symtbl' @@ get_formals header 
    in
    let symtbl''' = 
      match header with
      | H_proc _ -> symtbl''
      | H_func (_, (_, pcl_type)) -> add "result" (Variable pcl_type) symtbl''
    in
    typecheck_ast_body ~symtbl:(add_tbl symtbl symtbl''') body;
    symtbl'
  | Loc_decl header -> typecheck_add_ast_header false ~symtbl_loc header


(* Typechecks a block of statements *)
and typecheck_ast_block ~symtbl block = List.iter (typecheck_ast_stmt ~symtbl) block


(* Typechecks a routine's header and returns a symbol table
with the routine added to it *)
and typecheck_add_ast_header def ~symtbl_loc = function
  | H_proc (id, formals) ->
    check_duplicate_ids @@ id :: ids_of_formals formals;
    update_entry id (Procedure (formals, { def=def })) symtbl_loc
  | H_func (id, (formals, pcl_type)) ->
    check_duplicate_ids @@ id :: ids_of_formals formals;
    update_entry id (Function ((formals, pcl_type), { def=def })) symtbl_loc


(* Typechecks a statement *)
and typecheck_ast_stmt stmt ~symtbl =
  (* Checks that an id exists in the symbol table and is a label *)
  let check_label id =
    try
      match find id symtbl with
      | Label _ -> ()
      | _ ->
        raise @@ TypingError (Printf.sprintf "%s is not a label" id)
    with Not_found ->
      raise @@ TypingError (Printf.sprintf "Undeclared label %s" id) 
  in
  (* Marks a label as used. If the id is not in the symbol table or
  if is already marked as used it raises an exception *)
  let mark_label id =
    try
      match find id symtbl with
      | Label l when l.used = false -> l.used <- true
      | Label { used=true } ->
        raise @@ TypingError
          (Printf.sprintf "label %s is used more than once" id)
      | _ ->
        raise @@ TypingError (Printf.sprintf "%s is not a label" id)
    with Not_found ->
      raise @@ TypingError (Printf.sprintf "Undeclared label %s" id)
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
    let e_type = type_of_ast_expr expr ~symtbl in
    if is_assignable l_type e_type then () 
    else
      raise @@ TypingError
        (Printf.sprintf "%s is assigned an expression of incompatible type"
        @@ Ast_print.string_of_ast_lvalue lvalue)
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
    mark_label id;
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
          | Typ_pointer (Some Typ_array (None, _)) ->
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
          | Typ_pointer (Some Typ_array (None, _)) -> ()
          | _ ->
            raise @@ TypingError
              (Printf.sprintf
              "Cannot array-allocate %s, it is not a pointer to an incomplete array"
              @@ Ast_print.string_of_ast_lvalue lvalue)
        end
      | _ ->
        raise @@ TypingError
        (Printf.sprintf "%s is not integer. Cannot allocate non-integer amount memory"
        @@ Ast_print.string_of_ast_expr @@ Option.get expr)
    end
  | St_dispose (paren_opt, lvalue) ->
    let l_type = type_of_ast_lvalue lvalue ~symtbl in
    begin
      match paren_opt with
      | None ->
        begin
          match l_type with
          | Typ_pointer (Some Typ_array (None, _)) ->
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
        begin
          match l_type with
          | Typ_pointer (Some Typ_array (None, _)) -> ()
          | _ ->
            raise @@ TypingError
              (Printf.sprintf
              "Cannot array-deallocate %s, it is not a pointer to an incomplete array"
              @@ Ast_print.string_of_ast_lvalue lvalue)
        end
    end


and type_of_ast_lvalue ~symtbl = function
  | Lv_id id ->
    begin
      try
        match find id symtbl with
        | Variable pcl_type -> pcl_type
        | _ ->
          raise @@ TypingError (Printf.sprintf "%s is not a valid lvalue" id) 
      with Not_found ->
        raise @@ TypingError (Printf.sprintf "Undeclared variable %s" id) 
    end
  | Lv_result ->
    begin
      try
        match find "result" symtbl with
        | Variable pcl_type -> pcl_type
        | _ ->
          (* Should be unreachable *)
          failwith "This code should not be reachable. In function type_of_ast_lvalue"
      with Not_found ->
        raise @@ TypingError "Keyword result cannot be used here" 
    end
  | Lv_string s ->
    let len = String.length s + 1 in
    Typ_array (Some len, Typ_char)
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
      | Typ_pointer (Some pcl_type) -> pcl_type
      | Typ_pointer None ->
        raise @@ TypingError
        (Printf.sprintf "nil cannot be dereferenced")     
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
  | Rv_nil -> Typ_pointer None
  | Rv_call call ->
    begin
      match type_of_ast_call call ~symtbl with
      | Some pcl_type -> pcl_type
      | None -> 
        let { routine_name=id; _ } = call in
        raise @@ TypingError 
          (Printf.sprintf "Procedure %s cannot be called in expression" id)
    end
  | Rv_ref lvalue -> Typ_pointer (Some (type_of_ast_lvalue lvalue ~symtbl))
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
    if e_type <> Typ_int && e_type <> Typ_real then
      let s = Ast_print.string_of_unop unop in
      raise @@ TypingError (Printf.sprintf "Unary %s applied to non-arithmetic expession" s)
    else ()


and type_of_binop binop e1_type e2_type =
  let is_int = function
    | Typ_int -> true
    | _ -> false
  in
  let is_arithmetic = function
    | Typ_int | Typ_real -> true
    | _ -> false
  in
  let is_bool = function
    | Typ_bool -> true
    | _ -> false
  in
  match binop with
  | Bop_plus | Bop_minus | Bop_times ->
    if is_arithmetic e1_type && is_arithmetic e2_type 
    then 
      if e1_type = e2_type
      then e1_type
      else Typ_real
    else raise @@ TypingError 
      (Printf.sprintf "%s applied to non arithmetic type" @@ Ast_print.string_of_binop binop)
  | Bop_rdiv ->
    if is_arithmetic e1_type && is_arithmetic e2_type
    then Typ_real
    else raise @@ TypingError "/ applied to non arithmetic type"
  | Bop_div
  | Bop_mod ->
    if is_int e1_type && is_int e2_type
    then Typ_int
    else raise @@ TypingError "Type mismatch"
  | Bop_or
  | Bop_and ->
    if is_bool e1_type && is_bool e2_type
    then Typ_bool
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
    if is_arithmetic e1_type && e1_type = e2_type
    then Typ_bool
    else raise @@ TypingError "Type mismatch"


(* Typechecks a call to a routine and returns the result type.
If the routine is a procedure then None is returned. *)
and type_of_ast_call { routine_name; args } ~symtbl =
  let rec check_args = function
    | [], [] -> ()
    | _, [] | [], _ ->
      raise @@ TypingError
        (Printf.sprintf "Wrong number of arguments in call to %s" routine_name)
    | F_byval (_, pcl_type) :: formals, expr::exprs ->
      if is_assignable pcl_type @@ type_of_ast_expr expr ~symtbl
      then check_args (formals, exprs)
      else raise @@ TypingError "Type mismatch in routine call"
    | F_byref (_, pcl_type) :: formals, E_lvalue lvalue :: exprs ->
      if is_referencable pcl_type @@ type_of_ast_lvalue lvalue ~symtbl
      then check_args (formals, exprs)
      else raise @@ TypingError "Type mismatch in routine call"
    | _ ->
      raise @@ TypingError "Actual parameter of var declared formal must be lvalue"
  in
  try
    match find routine_name symtbl with
    | Function ((formals, pcl_type), _) ->
      check_args (formals, args);
      Some pcl_type
    | Procedure (formals, _) ->
      check_args (formals, args);
      None
    | _ ->
      raise @@ TypingError
        (Printf.sprintf "%s is not callable" routine_name)
  with Not_found ->
    raise @@ TypingError
      (Printf.sprintf "Undeclared routine %s" routine_name)
