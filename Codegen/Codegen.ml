open Llvm
open Symtbl
open Ast


let context = global_context ()
let the_module = create_module context "llvm program"
let builder = builder context


(* Hashtable mapping the ids of routines in the program to the
the names used by llvm. Is is needed due to name shadowing *)
let function_names : (string, string) Hashtbl.t = Hashtbl.create 10


(* lltypes for the basic pcl types *)
let void_type = void_type context
let int_type  = i64_type context
let real_type = x86fp80_type context
let bool_type = i8_type context
let char_type = i8_type context


(* Function mapping pcl types to lltypes *)
let rec lltype_of_pcl_type = function
  | Typ_int -> int_type
  | Typ_real -> real_type
  | Typ_bool -> bool_type
  | Typ_char -> char_type
  | Typ_pointer None -> pointer_type void_type
  | Typ_pointer (Some pcl_type) ->
    pointer_type @@ lltype_of_pcl_type pcl_type
  | Typ_array (Some n, pcl_type) ->
    array_type (lltype_of_pcl_type pcl_type) n
  | Typ_array (None, pcl_type) ->
    array_type (lltype_of_pcl_type pcl_type) 0


(* Generates a new name for a routine and adds it to the hashtable *)
let function_name_gen id =
  match Hashtbl.find_opt function_names id with
  (* If the id does not already exist then add it as it is *)
  | None -> Hashtbl.add function_names id id; id
  | Some old_id ->
    let func_llvalue = Option.get @@ lookup_function old_id the_module in
    match basic_blocks func_llvalue with
    (* If the function has no basic blocks then it was forward declared
    and is now implemented so return the already registered name *)
    | [||] -> old_id
    (* Otherwise append a '$' to the end of the previous name *)
    | _ ->
      let new_id = old_id ^ "$" in
        Hashtbl.add function_names id new_id; new_id


let lltype_of_header header =
  let arraify_formal_types formals =
    Array.of_list
    @@
    List.map
      (function | F_byref (_, pcl_type) -> pointer_type @@ lltype_of_pcl_type pcl_type
                | F_byval (_, pcl_type) -> lltype_of_pcl_type pcl_type)
      formals
  in
  match header with
  | H_func (_, (formals, pcl_type)) ->
    function_type (lltype_of_pcl_type pcl_type) (arraify_formal_types formals)
  | H_proc (_, formals) ->
    function_type void_type (arraify_formal_types formals)


(* let library_symtbl =
  let add_libfunc (name, args) =
    let fname = function_name_gen name in
    let args = Array.map lltype_of_pcl_type args in
    ()
  in
  () *)


let rec codegen { prog_name=_; body } =
  let main_type = function_type void_type [||] in
  let main_id = function_name_gen "main" in
  let main = declare_function main_id main_type the_module in
  let bb = append_block context "entry" main in
  position_at_end bb builder; 
  codegen_body body ~symtbl:empty;
  ignore @@ build_ret_void builder;
  the_module


and codegen_body { decls; block } ~symtbl =
  let symtbl' = List.fold_left (fun symtbl -> codegen_local ~symtbl) symtbl decls in
  codegen_block block ~symtbl:symtbl'


and codegen_header header ~symtbl = 
  let id =
    match header with
    | H_func (id, _) | H_proc (id, _) -> id 
  in
  let func_name = function_name_gen id in
  let header_type = lltype_of_header header in
  let func_val = declare_function func_name header_type the_module in
  (add id func_val symtbl, func_val)


and codegen_local ~symtbl = function
  | Loc_var vars ->
    let build_id symtbl (id, pcl_type) =
      let alloca_val = build_alloca (lltype_of_pcl_type pcl_type) id builder in
      add id alloca_val symtbl
    in
    List.fold_left build_id symtbl vars
  | Loc_label ids ->
    let parent = block_parent @@ insertion_block builder in
    let build_label symtbl id =
      let bb = value_of_block @@ append_block context id parent in
      add id bb symtbl
    in
    List.fold_left build_label symtbl ids
  | Loc_decl header -> fst @@ codegen_header header ~symtbl
  | Loc_def (header, body) ->
    let symtbl', func_val = codegen_header header ~symtbl in
    let cur_block = insertion_block builder in
    let entry_block = append_block context "entry" func_val in
    position_at_end entry_block builder;
    let symtbl'' = 
      match header with
      | H_proc _ -> symtbl'
      | H_func (_, (_, pcl_type)) ->
        let res = build_alloca (lltype_of_pcl_type pcl_type) "result_alloca" builder in
        add "result" res symtbl'
    in
    let formals =
      match header with
      | H_proc (_, formals)
      | H_func (_, (formals, _)) -> formals
    in
    let codegen_formal symtbl = function
      | F_byval (id, pcl_type), p ->
        let alloca_val = build_alloca (lltype_of_pcl_type pcl_type) "alloca" builder in
        let symtbl' = add id alloca_val symtbl in
        ignore @@ build_store p alloca_val builder;
        symtbl'
      | F_byref (id, _), p -> add id p symtbl    
    in
    let symtbl''' =
      List.fold_left codegen_formal symtbl'' @@ List.combine formals
      @@ Array.to_list @@ params func_val;
    in
    codegen_body body ~symtbl:symtbl''';
    let _ =
      match header with
      | H_func _ ->
        let res = build_load (find "result" symtbl'') "load" builder in
        build_ret res builder 
      | H_proc _ -> build_ret_void builder
    in
    position_at_end cur_block builder;
    symtbl'


and codegen_block ~symtbl block = List.iter (codegen_stmt ~symtbl) block


and codegen_stmt ~symtbl = function
  | St_empty -> ()
  | St_assign (lvalue, expr) ->
    let l_llvalue = codegen_lvalue lvalue ~symtbl in
    let l_lltype = element_type @@ type_of l_llvalue in
    let e_llvalue = codegen_expr expr ~symtbl in
    let e_llvalue' =
      if l_lltype == type_of e_llvalue
      then e_llvalue
      else build_bitcast e_llvalue l_lltype "bitcast" builder
    in
    ignore @@ build_store e_llvalue' l_llvalue builder
  | St_block block -> codegen_block ~symtbl block
  | St_call call -> ignore @@ codegen_call call ~symtbl
  | St_if (expr, then_stmt, else_stmt_opt) ->
    let cond = codegen_expr expr ~symtbl in
    let cur_bb = insertion_block builder in
    let parent = block_parent cur_bb in
    let cont_bb = append_block context "cont" parent in
    let build_block name stmt =
      let bb = append_block context name parent in
      position_at_end bb builder;
      codegen_stmt stmt ~symtbl;
      ignore @@ build_br cont_bb builder;
      bb
    in
    let then_bb = build_block "then" then_stmt in
    let else_bb = Option.(value ~default:cont_bb @@ map (build_block "else") else_stmt_opt) in
    move_block_after (insertion_block builder) cont_bb;
    position_at_end cur_bb builder;
    ignore @@ build_cond_br cond then_bb else_bb builder;
    position_at_end cont_bb builder
  | St_while (expr, stmt) ->
    let parent = block_parent @@ insertion_block builder in
    let cond_bb = append_block context "cond" parent in
    let while_bb = append_block context "while" parent in
    let cont_bb = append_block context "cont" parent in
    position_at_end cond_bb builder;
    let cond = codegen_expr expr ~symtbl in
    ignore @@ build_cond_br cond while_bb cont_bb builder;
    position_at_end while_bb builder;
    codegen_stmt stmt ~symtbl;
    ignore @@ build_br cond_bb builder;
    position_at_end cont_bb builder
  | St_label (id, stmt) ->
    let cur_bb = insertion_block builder in
    let bb = block_of_value @@ find id symtbl in
    move_block_after cur_bb bb;
    position_at_end bb builder;
    codegen_stmt stmt ~symtbl
  | St_goto id ->
    let bb = block_of_value @@ find id symtbl in
    ignore @@ build_br bb builder;
    let cur_bb = insertion_block builder in
    let parent = block_parent cur_bb in
    let next_bb = append_block context "next" parent in
    move_block_after cur_bb next_bb;
    position_at_end next_bb builder
  | St_return ->
    ignore @@
    let ret_type = return_type @@ type_of @@ block_parent @@ insertion_block builder in
    if ret_type = void_type then
      build_ret_void builder
    else
      let res = build_load (find "result" symtbl) "load" builder in
      build_ret res builder 
  | St_new (expr, lv) ->
    let l_llvalue = codegen_lvalue lv ~symtbl in
    let malloc_type = element_type @@ element_type @@ type_of l_llvalue in
    ignore @@
    let new_llvalue =
      match expr with
      | Some e ->
        let e_llvalue = codegen_expr e ~symtbl in
        build_array_malloc malloc_type e_llvalue "new" builder
      | None ->
        build_malloc malloc_type "new" builder
    in
    build_store new_llvalue l_llvalue builder
  | St_dispose (_, lv) ->
    let l_llvalue = codegen_lvalue lv ~symtbl in
    ignore @@ build_free l_llvalue builder


and codegen_lvalue ~symtbl = function
  | Lv_id id -> find id symtbl
  | Lv_result -> find "result" symtbl
  | Lv_string str -> build_global_string str "str" builder
  | Lv_array (lvalue, expr) ->
    let l_llvalue = codegen_lvalue lvalue ~symtbl in
    let e_llvalue = codegen_expr expr ~symtbl in
    let zero = const_int int_type 0 in
    build_gep l_llvalue [| zero; e_llvalue |] "gep" builder
  | Lv_deref expr -> codegen_expr expr ~symtbl


and codegen_rvalue ~symtbl = function
  | Rv_int i -> const_int int_type i
  | Rv_bool b -> const_int bool_type @@ Bool.to_int b
  | Rv_real f -> const_float real_type f
  | Rv_char c -> const_int char_type @@ Char.code c
  | Rv_nil -> const_pointer_null void_type
  | Rv_call call -> codegen_call call ~symtbl
  | Rv_ref lvalue -> codegen_lvalue lvalue ~symtbl
  | Rv_unop (unop, expr) -> codegen_unop unop expr ~symtbl
  | Rv_binop (lhs, binop, rhs) -> codegen_binop binop lhs rhs ~symtbl


and codegen_expr ~symtbl = function
  | E_lvalue lvalue -> build_load (codegen_lvalue ~symtbl lvalue) "load" builder
  | E_rvalue rvalue -> codegen_rvalue ~symtbl rvalue


and codegen_call { routine_name; args; } ~symtbl =
  let func_val = find routine_name symtbl in
  let codegen_arg = function
  | E_rvalue arg, _ -> codegen_rvalue arg ~symtbl
  | E_lvalue arg, param_type ->
    let l_llvalue = codegen_lvalue ~symtbl arg in
    let l_lltype = type_of l_llvalue in
    if element_type l_lltype == param_type
    then build_load l_llvalue "load" builder
    else if l_lltype == param_type
         then l_llvalue
         else build_bitcast l_llvalue param_type "bitcast" builder
  in
  let args_types = Array.to_list @@ Array.map type_of @@ params func_val in
  let args_array = Array.of_list @@ List.map codegen_arg @@ List.combine args args_types in
  build_call func_val args_array "call" builder


and codegen_unop unop expr ~symtbl =
  let e_llvalue = codegen_expr expr ~symtbl in
  match unop with
  | Uop_not -> build_not e_llvalue "not" builder
  | Uop_plus -> e_llvalue
  | Uop_minus -> build_neg e_llvalue "neg" builder


and codegen_binop binop lhs rhs ~symtbl =
  let lhs_llvalue = codegen_expr lhs ~symtbl in
  let rhs_llvalue = codegen_expr rhs ~symtbl in
  let lhs_ltype = type_of lhs_llvalue in
  let rhs_ltype = type_of rhs_llvalue in
  let build_arithmetic build_iop build_fop =
    if lhs_ltype = int_type && rhs_ltype = int_type then
      build_iop lhs_llvalue rhs_llvalue "iop" builder 
    else if lhs_ltype = int_type && rhs_ltype = real_type then
      let lhs_llvalue' = build_sitofp lhs_llvalue real_type "cast" builder in
      build_fop lhs_llvalue' rhs_llvalue "fop" builder
    else if lhs_ltype = real_type && rhs_ltype = int_type then
      let rhs_llvalue' = build_sitofp rhs_llvalue real_type "cast" builder in
      build_fop lhs_llvalue rhs_llvalue' "fop" builder
    else
      build_fop lhs_llvalue rhs_llvalue "fop" builder
  in
  match binop with
  | Bop_plus -> build_arithmetic build_add build_fadd
  | Bop_minus -> build_arithmetic build_sub build_fsub
  | Bop_times -> build_arithmetic build_mul build_fmul
  | Bop_rdiv -> build_fdiv lhs_llvalue rhs_llvalue "fdiv" builder
  | Bop_div -> build_sdiv lhs_llvalue rhs_llvalue "div" builder
  | Bop_mod -> build_srem lhs_llvalue rhs_llvalue "srem" builder
  | Bop_or -> build_or lhs_llvalue rhs_llvalue "or" builder
  | Bop_and -> build_and lhs_llvalue rhs_llvalue "and" builder
  | Bop_eq ->
    if type_of lhs_llvalue = int_type then
      build_icmp Icmp.Eq lhs_llvalue rhs_llvalue "eq" builder
    else
      build_fcmp Fcmp.Oeq lhs_llvalue rhs_llvalue "feq" builder
  | Bop_neq ->
    if type_of lhs_llvalue = int_type then
      build_icmp Icmp.Ne lhs_llvalue rhs_llvalue "neq" builder
    else
      build_fcmp Fcmp.One lhs_llvalue rhs_llvalue "fneq" builder
  | Bop_less ->
    if type_of lhs_llvalue = int_type then
      build_icmp Icmp.Slt lhs_llvalue rhs_llvalue "less" builder
    else
      build_fcmp Fcmp.Olt lhs_llvalue rhs_llvalue "fless" builder
  | Bop_leq ->
    if type_of lhs_llvalue = int_type then
      build_icmp Icmp.Sle lhs_llvalue rhs_llvalue "leq" builder
    else
      build_fcmp Fcmp.Ole lhs_llvalue rhs_llvalue "fleq" builder
  | Bop_greater ->
    if type_of lhs_llvalue = int_type then
      build_icmp Icmp.Sgt lhs_llvalue rhs_llvalue "gr" builder
    else
      build_fcmp Fcmp.Ogt lhs_llvalue rhs_llvalue "fgr" builder
  | Bop_geq ->
    if type_of lhs_llvalue = int_type then
      build_icmp Icmp.Sge lhs_llvalue rhs_llvalue "geq" builder
    else
      build_fcmp Fcmp.Oge lhs_llvalue rhs_llvalue "fgeq" builder
