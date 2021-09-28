open Ast
open Identifier
open Types
open Symtbl
open SemAst


exception SemanticError of string


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
          raise @@ SemanticError
            (Printf.sprintf "function %s declared more than once in a group of local declerations" id)
        | _ ->
          raise @@ SemanticError
            (Printf.sprintf "duplicate id %s" id)
      end
    | Some Procedure (proc_type1, { def=false }) ->
      begin
        match sym_entry with
        | Procedure (proc_type2, { def=true }) when proc_type1 = proc_type2 -> Some sym_entry
        | Procedure (_, _) -> 
          raise @@ SemanticError
            (Printf.sprintf "procedure %s declared more than once in a group of local declerations" id)
        | _ ->
          raise @@ SemanticError
            (Printf.sprintf "duplicate id %s" id)
      end
    | _ ->
      raise @@ SemanticError
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
      raise @@ SemanticError (Printf.sprintf "Duplicate definition of %s" id)
  in
  ignore @@ List.fold_left try_add IdSet.empty ids


(* Checks whether a value of rhs_type can be assigned to a value of lhs type *)
let is_assignable lhs_type rhs_type =
  match lhs_type, rhs_type with
  | Typ_real, Typ_int -> true
  | Typ_pointer (Some _), Typ_pointer None
  | Typ_pointer None, Typ_pointer (Some _) -> true
  | Typ_pointer (Some Typ_array (None, pcl_type1)), Typ_pointer (Some Typ_array (_, pcl_type2)) ->
    pcl_type1 = pcl_type2
  | Typ_pointer (Some Typ_array (None, _)), pcl_type ->
    raise @@ SemanticError
     (Printf.sprintf "Value of type %s cannot be assigned to incomplete array"
     @@ AstPrint.string_of_type pcl_type)
  | pcl_type1, pcl_type2 -> pcl_type1 = pcl_type2


(* Checks whether an a value of rhs_type can be passed by reference
to a formal argument of type lhs_type *)
let is_referencable lhs_type rhs_type =
  is_assignable (Typ_pointer (Some lhs_type)) (Typ_pointer (Some rhs_type))


(* Typechecks the abstact syntax tree *)
let rec typecheck_ast { prog_name; body } =
  let sem_body = typecheck_ast_body ~symtbl:library_symtbl body in
  { sem_prog_name = prog_name; sem_body }


(* Typechecks the body of program and routines.
Raises an exception in case of error *)
and typecheck_ast_body ~symtbl { decls; block } =
  let sem_vars, sem_labels, sem_routines, symtbl_loc =
    decls
    |> List.fold_left (typecheck_add_ast_local ~symtbl) ([], [], [], empty)
    |> (fun (a, b, c, d) -> (List.rev a, List.rev b, List.rev c, d))
  in
  let _false = (* Check for undefined routines *)
    exists
    (fun id -> function
      | Procedure (_, { def=false }) ->
        raise @@ SemanticError (Printf.sprintf "Undefined procedure %s" id)
      | Function  (_, { def=false }) ->
        raise @@ SemanticError (Printf.sprintf "Undefined function %s" id)
      | _ -> false
    )
    symtbl_loc
  in
  let sem_block = typecheck_ast_block ~symtbl:(add_tbl symtbl symtbl_loc) block in
  let _false = (* Check for unused labels *)
    exists
    (fun id -> function
      | Label { used=false } -> raise @@ SemanticError (Printf.sprintf "Unused label %s" id)
      | _ -> false
    )
    symtbl_loc
  in
  {
    sem_vars;
    sem_labels;
    sem_routines;
    sem_block
  }


(* Typechecks local declarations and returns an updated symbol table.
symtlb_loc is needed to detect duplicate definitions whilst ignoring
name shadowing *)
and typecheck_add_ast_local ~symtbl (loc_vars, loc_labels, loc_routines, symtbl_loc) = function
  | Loc_var ((id, pcl_type) as var) ->
    let symtbl_loc' = update_entry id (Variable pcl_type) symtbl_loc in
    (var::loc_vars, loc_labels, loc_routines, symtbl_loc')
  | Loc_label id ->
    let symtbl_loc' = update_entry id (Label { used=false }) symtbl_loc in
    (loc_vars, id::loc_labels, loc_routines, symtbl_loc')
  | Loc_def (header, body) ->
    let symtbl_loc' = typecheck_add_ast_header true ~symtbl_loc header in
    let symtbl' =
      filter
      (fun _ -> function
        | Procedure _ -> true
        | Function _ -> true
        | _ -> false
      )
      symtbl_loc'
    in
    let formals =
      match header with
      | H_proc (_, formals) -> formals
      | H_func (_, (formals, _)) -> formals
    in
    let add_formal symtbl = function
      | F_byref (id, pcl_type)
      | F_byval (id, pcl_type) -> add id (Variable pcl_type) symtbl
    in
    let symtbl'' =
      List.fold_left add_formal symtbl' formals
    in
    let symtbl''' = 
      match header with
      | H_proc _ -> symtbl''
      | H_func (_, (_, pcl_type)) -> add "result" (Variable pcl_type) symtbl''
    in
    let sem_body = typecheck_ast_body ~symtbl:(add_tbl symtbl symtbl''') body in
    let sem_routine =
      match header with
      | H_proc (id, proc_type) -> Sem_proc (id, proc_type, sem_body)
      | H_func (id, func_type) -> Sem_func (id, func_type, sem_body)
    in
    (loc_vars, loc_labels, sem_routine::loc_routines, symtbl_loc')
  | Loc_decl header ->
    let symtbl_loc' = typecheck_add_ast_header false ~symtbl_loc header in
    (loc_vars, loc_labels, loc_routines, symtbl_loc')


(* Typechecks a block of statements *)
and typecheck_ast_block ~symtbl block = List.map (typecheck_ast_stmt ~symtbl) block


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
        raise @@ SemanticError (Printf.sprintf "%s is not a label" id)
    with Not_found ->
      raise @@ SemanticError (Printf.sprintf "Undeclared label %s" id) 
  in
  (* Marks a label as used. If the id is not in the symbol table or
  if is already marked as used it raises an exception *)
  let mark_label id =
    try
      match find id symtbl with
      | Label l when l.used = false -> l.used <- true
      | Label { used=true } ->
        raise @@ SemanticError
          (Printf.sprintf "label %s is used more than once" id)
      | _ ->
        raise @@ SemanticError (Printf.sprintf "%s is not a label" id)
    with Not_found ->
      raise @@ SemanticError (Printf.sprintf "Undeclared label %s" id)
  in
  let check_bool expr =
    let sem_expr, e_type = typecheck_ast_expr expr ~symtbl in
    match e_type with
    | Typ_bool -> sem_expr
    | _ ->
      raise @@ SemanticError
        (Printf.sprintf "This expression is not boolean: %s"
        @@ AstPrint.string_of_ast_expr expr)
  in
  match stmt with
  | St_empty -> SemSt_empty
  | St_assign (lvalue, expr) ->
    let sem_lvalue, l_type = typecheck_ast_lvalue lvalue ~symtbl in
    let sem_expr, e_type = typecheck_ast_expr expr ~symtbl in
    if is_assignable l_type e_type then SemSt_assign (sem_lvalue, sem_expr)
    else
      raise @@ SemanticError
        (Printf.sprintf "%s is assigned an expression of incompatible type"
        @@ AstPrint.string_of_ast_lvalue lvalue)
  | St_block block -> SemSt_block (typecheck_ast_block block ~symtbl)
  | St_call call -> SemSt_call (typecheck_ast_call call ~symtbl)
  | St_if (expr, if_stmt, else_stmt) ->
    let sem_expr = check_bool expr in
    let sem_if = typecheck_ast_stmt if_stmt ~symtbl in
    let sem_else = Option.map (typecheck_ast_stmt ~symtbl) else_stmt in
    SemSt_if (sem_expr, sem_if, sem_else)
  | St_while (expr, stmt) ->
    let sem_expr = check_bool expr in
    let sem_stmt = typecheck_ast_stmt stmt ~symtbl in
    SemSt_while (sem_expr, sem_stmt)
  | St_label (id, stmt) ->
    mark_label id;
    let sem_stmt = typecheck_ast_stmt stmt ~symtbl in
    SemSt_label (id, sem_stmt)
  | St_goto id ->
    check_label id;
    SemSt_goto id
  | St_return -> SemSt_return
  | St_new (expr, lvalue) ->
    let sem_lvalue, l_type = typecheck_ast_lvalue lvalue ~symtbl in
    begin
      match Option.map (typecheck_ast_expr ~symtbl) expr with
      | None ->
        begin
          match l_type with
          | Typ_pointer (Some Typ_array (None, _)) ->
            raise @@ SemanticError
              (Printf.sprintf
              "%s is a pointer to an incomplete array and should be array-allocated"
              @@ AstPrint.string_of_ast_lvalue lvalue)
          | Typ_pointer _ -> SemSt_new sem_lvalue
          | _ ->
            raise @@ SemanticError
              (Printf.sprintf
              "Cannot allocate for %s, it is not a pointer"
              @@ AstPrint.string_of_ast_lvalue lvalue)
        end
      | Some (sem_expr, Typ_int) ->
        begin
          match l_type with
          | Typ_pointer (Some Typ_array (None, _)) -> SemSt_new_array (sem_expr, sem_lvalue)
          | _ ->
            raise @@ SemanticError
              (Printf.sprintf
              "Cannot array-allocate %s, it is not a pointer to an incomplete array"
              @@ AstPrint.string_of_ast_lvalue lvalue)
        end
      | _ ->
        raise @@ SemanticError
        (Printf.sprintf "%s is not integer. Cannot allocate non-integer amount memory"
        @@ AstPrint.string_of_ast_expr @@ Option.get expr)
    end
  | St_dispose (paren_opt, lvalue) ->
    let sem_lvalue, l_type = typecheck_ast_lvalue lvalue ~symtbl in
    begin
      match paren_opt with
      | None ->
        begin
          match l_type with
          | Typ_pointer (Some Typ_array (None, _)) ->
            raise @@ SemanticError
              (Printf.sprintf
              "%s is a pointer to an incomplete array and should be array-deallocated"
              @@ AstPrint.string_of_ast_lvalue lvalue)
          | Typ_pointer _ -> SemSt_dispose sem_lvalue
          | _ ->
            raise @@ SemanticError
              (Printf.sprintf
              "Cannot deallocate %s, it is not a pointer"
              @@ AstPrint.string_of_ast_lvalue lvalue)
        end
      | Some () ->
        begin
          match l_type with
          | Typ_pointer (Some Typ_array (None, _)) -> SemSt_dispose_array sem_lvalue
          | _ ->
            raise @@ SemanticError
              (Printf.sprintf
              "Cannot array-deallocate %s, it is not a pointer to an incomplete array"
              @@ AstPrint.string_of_ast_lvalue lvalue)
        end
    end


and typecheck_ast_expr ~symtbl = function
  | E_lvalue lvalue ->
    let sem_lvalue, e_type =  typecheck_ast_lvalue lvalue ~symtbl in
    SemE_lvalue (sem_lvalue, e_type), e_type
  | E_rvalue rvalue ->
    let sem_rvalue, e_type =  typecheck_ast_rvalue rvalue ~symtbl in
    SemE_rvalue (sem_rvalue, e_type), e_type


and typecheck_ast_lvalue ~symtbl = function
  | Lv_id id ->
    begin
      try
        match find id symtbl with
        | Variable pcl_type -> (SemLv_id id, pcl_type)
        | _ ->
          raise @@ SemanticError (Printf.sprintf "%s is not a valid lvalue" id) 
      with Not_found ->
        raise @@ SemanticError (Printf.sprintf "Undeclared variable %s" id) 
    end
  | Lv_result ->
    begin
      try
        match find "result" symtbl with
        | Variable pcl_type -> (SemLv_result, pcl_type)
        | _ ->
          (* Should be unreachable *)
          failwith "This code should not be reachable. In function type_of_ast_lvalue"
      with Not_found ->
        raise @@ SemanticError "Keyword result cannot be used here" 
    end
  | Lv_string s ->
    let len = String.length s + 1 in
    (SemLv_string s, Typ_array (Some len, Typ_char))
  | Lv_array (lvalue, expr) ->
    let sem_expr, e_type = typecheck_ast_expr expr ~symtbl in 
    let () =
      match e_type with
      | Typ_int -> ()
      | _ ->
        raise @@ SemanticError "Index expression should be integer"
    in
    let sem_lvalue, l_type = typecheck_ast_lvalue lvalue ~symtbl in
    begin
      match l_type with
      | Typ_array (_, pcl_type) -> (SemLv_array ((sem_lvalue, l_type), sem_expr) ,pcl_type)
      | _ ->
        raise @@ SemanticError
        (Printf.sprintf "%s is not an array and it cannot be indexed"
        @@ AstPrint.string_of_ast_lvalue lvalue)
    end
  | Lv_deref expr ->
    let sem_expr, e_type = typecheck_ast_expr expr ~symtbl in
    begin
      match e_type with
      | Typ_pointer (Some pcl_type) -> (SemLv_deref sem_expr, pcl_type)
      | Typ_pointer None ->
        raise @@ SemanticError
        (Printf.sprintf "nil cannot be dereferenced")     
      | _ ->
        raise @@ SemanticError
        (Printf.sprintf "%s is not a pointer and cannot be dereferenced"
        @@ AstPrint.string_of_ast_expr expr)      
    end


and typecheck_ast_rvalue ~symtbl = function
  | Rv_int i -> SemRv_int i, Typ_int
  | Rv_bool b -> SemRv_bool b, Typ_bool
  | Rv_real r -> SemRv_real r, Typ_real
  | Rv_char c -> SemRv_char c, Typ_char
  | Rv_nil -> SemRv_nil, Typ_pointer None
  | Rv_call call ->
    let sem_call = typecheck_ast_call call ~symtbl in
    let { sem_routine_name; sem_type; _ } = sem_call in
    begin
      match sem_type with
      | Some pcl_type -> SemRv_call sem_call, pcl_type
      | None -> 
        raise @@ SemanticError 
          (Printf.sprintf "Procedure %s cannot be called in expression" sem_routine_name)
    end
  | Rv_ref lvalue ->
    let sem_lvalue, pcl_type = typecheck_ast_lvalue lvalue ~symtbl in
    SemRv_ref (sem_lvalue, pcl_type), Typ_pointer (Some pcl_type)
  | Rv_unop (unop, expr) ->
    let sem_expr, e_type = typecheck_ast_expr expr ~symtbl in
    let e_type' = typecheck_unop unop e_type in
    SemRv_unop (unop, sem_expr), e_type'
  | Rv_binop (expr1, binop, expr2) ->
    let sem_expr1, e1_type = typecheck_ast_expr expr1 ~symtbl in
    let sem_expr2, e2_type = typecheck_ast_expr expr2 ~symtbl in
    let b_type = typecheck_binop binop e1_type e2_type in
    SemRv_binop (sem_expr1, binop, sem_expr2), b_type


and typecheck_unop unop e_type =
  match unop with
  | Uop_not ->
    if e_type <> Typ_bool then
      raise @@ SemanticError "Logical nagation applied to non-boolean expession"
    else Typ_bool
  | Uop_plus | Uop_minus as unop ->
    if e_type <> Typ_int && e_type <> Typ_real then
      let s = AstPrint.string_of_unop unop in
      raise @@ SemanticError (Printf.sprintf "Unary %s applied to non-arithmetic expession" s)
    else e_type


and typecheck_binop binop e1_type e2_type =
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
    else raise @@ SemanticError 
      (Printf.sprintf "%s applied to non arithmetic type" @@ AstPrint.string_of_binop binop)
  | Bop_rdiv ->
    if is_arithmetic e1_type && is_arithmetic e2_type
    then Typ_real
    else raise @@ SemanticError "/ applied to non arithmetic type"
  | Bop_div
  | Bop_mod ->
    if is_int e1_type && is_int e2_type
    then Typ_int
    else raise @@ SemanticError "Type mismatch"
  | Bop_or
  | Bop_and ->
    if is_bool e1_type && is_bool e2_type
    then Typ_bool
    else raise @@ SemanticError "Type mismatch"
  | Bop_eq | Bop_neq ->
    if is_array e1_type || is_array e2_type
    then raise @@ SemanticError "Cannot check equality for array type";
    
    if is_arithmetic e1_type && is_arithmetic e2_type
    then Typ_bool
    else if e1_type = e2_type
    then Typ_bool
    else raise @@ SemanticError "Type mismatch in equality or non-equality"
  | Bop_less | Bop_leq | Bop_greater | Bop_geq ->
    if is_arithmetic e1_type && e1_type = e2_type
    then Typ_bool
    else raise @@ SemanticError "Cannot compare non-arithmetic types"


(* Typechecks a call to a routine and returns the result type.
If the routine is a procedure then None is returned. *)
and typecheck_ast_call { routine_name; args } ~symtbl =
  let rec check_args = function
    | [], [] -> []
    | _, [] | [], _ ->
      raise @@ SemanticError
        (Printf.sprintf "Wrong number of arguments in call to %s" routine_name)
    | (F_byval (_, pcl_type) as formal) :: formals, expr::exprs ->
      let sem_expr, e_type = typecheck_ast_expr expr ~symtbl in
      if is_assignable pcl_type @@ e_type
      then (sem_expr, formal) :: check_args (formals, exprs)
      else raise @@ SemanticError "Type mismatch in routine call"
    | (F_byref (_, pcl_type) as formal) :: formals, E_lvalue lvalue :: exprs ->
      let sem_lvalue, l_type = typecheck_ast_lvalue lvalue ~symtbl in
      if is_referencable pcl_type @@ l_type
      then (SemE_lvalue (sem_lvalue, l_type), formal) :: check_args (formals, exprs)
      else raise @@ SemanticError "Type mismatch in routine call"
    | _ ->
      raise @@ SemanticError "Actual parameter of var declared formal must be lvalue"
  in
  try
    match find routine_name symtbl with
    | Function ((formals, pcl_type), _) ->
      let sem_args = check_args (formals, args) in
      { sem_routine_name = routine_name; sem_args; sem_type = Some pcl_type }
    | Procedure (formals, _) ->
      let sem_args = check_args (formals, args) in
      { sem_routine_name = routine_name; sem_args; sem_type = None }
    | _ ->
      raise @@ SemanticError
        (Printf.sprintf "%s is not callable" routine_name)
  with Not_found ->
    raise @@ SemanticError
      (Printf.sprintf "Undeclared routine %s" routine_name)
