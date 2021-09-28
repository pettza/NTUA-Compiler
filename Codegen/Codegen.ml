open Llvm
open Symtbl
open SemAst
open Types


let context = global_context ()
let the_module = create_module context "llvm program"
let builder = builder context


(* lltypes for the basic pcl types *)
let void_type = void_type context
let int_type  = i64_type context
let real_type = x86fp80_type context
let bool_type = i8_type context
let char_type = i8_type context
(* Conditions in branch instructions need to be of i1 type *)
let cond_type = i1_type context


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


(* LLVM function names need to be unique so they are generated with this function
It gets the name of the parent function and appends the given id after a period. *)
let function_name_gen id =
  let parent_name = value_name @@ block_parent @@ insertion_block builder in
  parent_name ^ "." ^ id


(* Returns an array with the lltypes of the formal arguments of a routine *)
let arraify_formal_types formals =
  Array.of_list
  @@
  List.map
    (function | F_byref (_, pcl_type) -> pointer_type @@ lltype_of_pcl_type pcl_type
              | F_byval (_, pcl_type) -> lltype_of_pcl_type pcl_type)
    formals


let lltype_of_func_type (formals, pcl_type) =
  function_type (lltype_of_pcl_type pcl_type) (arraify_formal_types formals)


let lltype_of_proc_type formals =
  function_type void_type (arraify_formal_types formals)


(* Declares the library routines and returns a symbol table mapping
their names to their respective llvalues *)
let gen_library_routines () =
  let llvalue_of_sym_entry id sym_entry =
    let lltype = match sym_entry with
    | Function (func_type, _) -> lltype_of_func_type func_type 
    | Procedure (proc_type, _) -> lltype_of_proc_type proc_type
    | _ -> failwith "This should be unreachable. Non-routine entry in library symbol table"
    in
    let llvalue = declare_function id lltype the_module in
    set_linkage Linkage.External llvalue;
    llvalue
  in
  Symtbl.mapi llvalue_of_sym_entry library_symtbl


(* Builds alloca instructions for variables and returns the updated symbol table *)
let codegen_vars vars ~symtbl =
  (* Variables are pointers to the stack where the data is allocated *)
  let build_id symtbl (id, pcl_type) =
      let alloca_val = build_alloca (lltype_of_pcl_type pcl_type) id builder in
      add id alloca_val symtbl
  in
  List.fold_left build_id symtbl vars


(* Creates blocks for the labels and returns the updated symbol table *)
let codegen_labels labels ~symtbl =
  let parent = block_parent @@ insertion_block builder in
  let parent_name = value_name parent in
  let build_label symtbl label =
    let block_name = parent_name ^ "$" ^ label in
    let bb = value_of_block @@ append_block context block_name parent in
    add label bb symtbl
  in
  List.fold_left build_label symtbl labels


(* Defines the routines without generating their code. Returns the updated symbol table *)
let define_routines routines ~symtbl =
  let define_routine symtbl routine =
    let id, llfunc_type =
      match routine with
      | Sem_proc (id, proc_type, _) -> id, lltype_of_proc_type proc_type
      | Sem_func (id, func_type, _) -> id, lltype_of_func_type func_type
    in
    let func_name = function_name_gen id in
    let llvalue = define_function func_name llfunc_type the_module in
    add id llvalue symtbl
  in
  List.fold_left define_routine symtbl routines


(* Generates code for handling the arguments of a routine.
Returns the updated symbol table *)
let codegen_formals formals func_val ~symtbl =
  let codegen_formal symtbl = function
    (* Arguments that are passed by value are copied to the stack so that they may be modified *)
    | F_byval (id, pcl_type), p ->
      let alloca_val = build_alloca (lltype_of_pcl_type pcl_type) "alloca" builder in
      ignore @@ build_store p alloca_val builder;
      add id alloca_val symtbl
    (* Arguments that are passed by reference are in essence const pointers
    to the memory where the data is allocated. Since they are const they don't
    need to be allocated on the stack *)
    | F_byref (id, _), p -> add id p symtbl
  in
  List.fold_left codegen_formal symtbl @@
  List.combine formals @@ Array.to_list @@ params func_val


let rec codegen { sem_prog_name=_; sem_body } =
  let main_type = function_type void_type [||] in
  let main = declare_function "main" main_type the_module in
  let bb = append_block context "entry" main in
  position_at_end bb builder; 
  codegen_body sem_body ~symtbl:(gen_library_routines ());
  ignore @@ build_ret_void builder;
  the_module


and codegen_body { sem_vars; sem_labels; sem_routines; sem_block } ~symtbl =
  let symtbl' = define_routines sem_routines ~symtbl in
  List.iter (codegen_routine ~symtbl:symtbl') sem_routines;
  let symtbl'' = codegen_labels sem_labels ~symtbl:symtbl' in
  let symtbl''' = codegen_vars sem_vars ~symtbl:symtbl'' in
  codegen_block sem_block ~symtbl:symtbl'''


and codegen_routine routine ~symtbl =
    let id, formals, ret_opt, body =
      match routine with
      | Sem_proc (id, formals, body) -> id, formals, None, body
      | Sem_func (id, (formals, pcl_type), body) -> id, formals, Some pcl_type, body
    in
    (* Store current block so that the builder can be repositioned there *)
    let cur_block = insertion_block builder in
    (* Position bulder at the entry point of the function being generated *)
    let func_val = find id symtbl in
    position_at_end (entry_block func_val) builder;
    (* Allocate result variable if necessary *)
    let symtbl' =
      match ret_opt with
      | None -> symtbl
      | Some pcl_type ->
        let res = build_alloca (lltype_of_pcl_type pcl_type) "result_alloca" builder in
        add "result" res symtbl
    in
    let symtbl'' = codegen_formals formals func_val ~symtbl:symtbl' in
    codegen_body body ~symtbl:symtbl'';
    (* Build return instruction if necessary *)
    ignore @@
    begin
      match ret_opt with
      | None -> build_ret_void builder
      | Some _ ->
        let res = build_load (find "result" symtbl'') "load" builder in
        build_ret res builder
    end;
    (* Position builder where it was *)
    position_at_end cur_block builder


and codegen_block ~symtbl block = List.iter (codegen_stmt ~symtbl) block


and codegen_stmt ~symtbl = function
  | SemSt_empty -> ()
  | SemSt_assign (lvalue, expr) ->
    (* Codegen llvalues both sides *)
    let l_llvalue = codegen_lvalue lvalue ~symtbl in
    let l_lltype = element_type @@ type_of l_llvalue in
    let e_llvalue = codegen_expr expr ~symtbl in
    (* Bitcast between types if necessary *)
    let e_llvalue' =
      if l_lltype == type_of e_llvalue
      then e_llvalue
      else if l_lltype == real_type
      then build_sitofp e_llvalue real_type "cast" builder
      else build_bitcast e_llvalue l_lltype "bitcast" builder
    in
    ignore @@ build_store e_llvalue' l_llvalue builder
  | SemSt_block block -> codegen_block ~symtbl block
  | SemSt_call call -> ignore @@ codegen_call call ~symtbl
  | SemSt_if (expr, then_stmt, else_stmt_opt) ->
    (* Trunc is needed because llvm requires i1 type for conditions in branch instructions *)
    let cond = build_trunc (codegen_expr expr ~symtbl) cond_type "trunc" builder in
    let cur_bb = insertion_block builder in
    let parent = block_parent cur_bb in
    let cont_bb = append_block context "cont" parent in
    (* Builds a basic block that containts stmt and jump to the continuation block *)
    let build_block name stmt =
      let bb = append_block context name parent in
      position_at_end bb builder;
      codegen_stmt stmt ~symtbl;
      ignore @@ build_br cont_bb builder;
      bb
    in
    let then_bb = build_block "then" then_stmt in
    (* Optionally build else block if there is any, otherwise use the cont block *)
    let else_bb = Option.(value ~default:cont_bb @@ map (build_block "else") else_stmt_opt) in
    (* Use instertion block here because we don't know if this is the then block or the else block *)
    move_block_after (insertion_block builder) cont_bb;
    position_at_end cur_bb builder;
    (* Having set up the other blocks, finally build the branch instruction *)
    ignore @@ build_cond_br cond then_bb else_bb builder;
    position_at_end cont_bb builder
  | SemSt_while (expr, stmt) ->
    let parent = block_parent @@ insertion_block builder in
    (* The basic block evaluating the condition *)
    let cond_bb = append_block context "cond" parent in
    (* The basic block containing the body of the while loop *)
    let while_bb = append_block context "while" parent in
    (* The basic block the comes after *)
    let cont_bb = append_block context "cont" parent in
    ignore @@ build_br cond_bb builder;
    position_at_end cond_bb builder;
    (* Trunc is needed because llvm requires i1 type for conditions in branch instructions *)
    let cond = build_trunc (codegen_expr expr ~symtbl) cond_type "trunc" builder in
    ignore @@ build_cond_br cond while_bb cont_bb builder;
    position_at_end while_bb builder;
    codegen_stmt stmt ~symtbl;
    (* Unconditional brach to the condition block *)
    ignore @@ build_br cond_bb builder;
    position_at_end cont_bb builder
  | SemSt_label (id, stmt) ->
    let cur_bb = insertion_block builder in
    let bb = block_of_value @@ find id symtbl in
    (* Unconditional branch instuction needed to terminate the current block *)
    ignore @@ build_br bb builder;
    move_block_after cur_bb bb;
    position_at_end bb builder;
    codegen_stmt stmt ~symtbl
  | SemSt_goto id ->
    (* Build unconditional brach to label... *)
    let bb = block_of_value @@ find id symtbl in
    ignore @@ build_br bb builder;
    (* ...and create the basic block for the next statements *)
    let cur_bb = insertion_block builder in
    let parent = block_parent cur_bb in
    let next_bb = append_block context "next" parent in
    move_block_after cur_bb next_bb;
    position_at_end next_bb builder
  | SemSt_return ->
    ignore @@
    (* Find the return type of the (llvm) function *)
    let ret_type = return_type @@ type_of @@ block_parent @@ insertion_block builder in
    (* If it is a procedure then return void *)
    if ret_type = void_type then
      build_ret_void builder
    (* If it is a (pcl) function return the result *)
    else
      let res = build_load (find "result" symtbl) "load" builder in
      build_ret res builder
  | SemSt_new lv ->
    let l_llvalue = codegen_lvalue lv ~symtbl in
    (* Variables are essentially pointers to the stack where the pointers reside,
    that's why element_type is called 2 times *)
    let malloc_type = element_type @@ element_type @@ type_of l_llvalue in
    let new_llvalue = build_malloc malloc_type "new" builder in
    ignore @@ build_store new_llvalue l_llvalue builder
  | SemSt_new_array (expr, lv) ->
    let l_llvalue = codegen_lvalue lv ~symtbl in
    (* element_type is called 2 times for the same reason as for new *)
    let malloc_type = element_type @@ element_type @@ type_of l_llvalue in
    let e_llvalue = codegen_expr expr ~symtbl in
    let new_llvalue = build_array_malloc malloc_type e_llvalue "new_array" builder in
    ignore @@ build_store new_llvalue l_llvalue builder
  | SemSt_dispose lv ->
    let l_llvalue = codegen_lvalue lv ~symtbl in
    let ptr = build_load l_llvalue "load_ptr" builder in
    ignore @@ build_free ptr builder
  | SemSt_dispose_array lv ->
    let l_llvalue = codegen_lvalue lv ~symtbl in
    let ptr = build_load l_llvalue "load_ptr" builder in
    ignore @@ build_free ptr builder


and codegen_lvalue ~symtbl = function
  | SemLv_id id -> find id symtbl
  | SemLv_result -> find "result" symtbl
  | SemLv_string str ->
    let llvalue = build_global_string str "str" builder in
    set_global_constant false llvalue;
    llvalue
  | SemLv_array ((lvalue, _), expr) ->
    let l_llvalue = codegen_lvalue lvalue ~symtbl in
    let e_llvalue = codegen_expr expr ~symtbl in
    let zero = const_int int_type 0 in
    build_gep l_llvalue [| zero; e_llvalue |] "gep" builder
  | SemLv_deref expr -> codegen_expr expr ~symtbl


and codegen_rvalue ~symtbl = function
  | SemRv_int i -> const_int int_type i
  | SemRv_bool b -> const_int bool_type @@ Bool.to_int b
  | SemRv_real f -> const_float real_type f
  | SemRv_char c -> const_int char_type @@ Char.code c
  | SemRv_nil -> const_pointer_null void_type
  | SemRv_call call -> codegen_call call ~symtbl
  | SemRv_ref (lvalue, _) -> codegen_lvalue lvalue ~symtbl
  | SemRv_unop (unop, expr) -> codegen_unop unop expr ~symtbl
  | SemRv_binop (lhs, binop, rhs) -> codegen_binop binop lhs rhs ~symtbl


and codegen_expr ~symtbl = function
  | SemE_lvalue (lvalue, _) -> build_load (codegen_lvalue ~symtbl lvalue) "load" builder
  | SemE_rvalue (rvalue, _) -> codegen_rvalue ~symtbl rvalue


and codegen_call { sem_routine_name; sem_args; sem_type } ~symtbl =
  let func_val = find sem_routine_name symtbl in
  let codegen_arg = function
    | (SemE_lvalue (arg, pcl_type), F_byref _), param_type ->
      let l_llvalue = codegen_lvalue arg ~symtbl in
      (* Complete arrays are passed by reference to foramls with incoplete array type
      in that case a bitcast is needed *)
      if lltype_of_pcl_type pcl_type == param_type
      then l_llvalue
      else build_bitcast l_llvalue param_type "bitcast_byref_arg" builder
    | (sem_expr, F_byval _), param_type ->
      let e_llvalue = codegen_expr ~symtbl sem_expr in
      if param_type == real_type && type_of e_llvalue == int_type 
      then build_sitofp e_llvalue real_type "real_cast" builder
      else e_llvalue
    | _ -> failwith "In codegen_call. This case should have been characterized as a semantic error"
  in
  let args_types = Array.to_list @@ Array.map type_of @@ params func_val in
  let args_array = Array.of_list @@ List.map codegen_arg @@ List.combine sem_args args_types in
  (* LLVM complains if we name a call to a function returning void so we do the following *)
  let call_name = if Option.is_none sem_type then "" else "call" in
  build_call func_val args_array call_name builder


and codegen_unop unop expr ~symtbl =
  let e_llvalue = codegen_expr expr ~symtbl in
  match unop with
  | Uop_not -> build_not e_llvalue "not" builder
  | Uop_plus -> e_llvalue
  | Uop_minus -> build_neg e_llvalue "neg" builder


and codegen_binop binop lhs rhs ~symtbl =
  let lhs_llvalue = codegen_expr lhs ~symtbl in
  let rhs_llvalue = codegen_expr rhs ~symtbl in
  let lhs_lltype = type_of lhs_llvalue in
  let rhs_lltype = type_of rhs_llvalue in
  let build_arithmetic build_iop build_fop =
    if lhs_lltype = int_type && rhs_lltype = int_type then
      build_iop lhs_llvalue rhs_llvalue "iop" builder 
    else if lhs_lltype = int_type && rhs_lltype = real_type then
      let lhs_llvalue' = build_sitofp lhs_llvalue real_type "cast" builder in
      build_fop lhs_llvalue' rhs_llvalue "fop" builder
    else if lhs_lltype = real_type && rhs_lltype = int_type then
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
    if lhs_lltype = int_type || lhs_lltype = real_type
    then build_arithmetic (build_icmp Icmp.Eq) (build_fcmp Fcmp.Oeq)
    else build_icmp Icmp.Eq lhs_llvalue rhs_llvalue "eq" builder
  | Bop_neq ->
    if lhs_lltype = int_type || lhs_lltype = real_type
    then build_arithmetic (build_icmp Icmp.Ne) (build_fcmp Fcmp.One)
    else build_icmp Icmp.Ne lhs_llvalue rhs_llvalue "neq" builder
  | Bop_less ->
    build_arithmetic (build_icmp Icmp.Slt) (build_fcmp Fcmp.Olt)
  | Bop_leq ->
    build_arithmetic (build_icmp Icmp.Sle) (build_fcmp Fcmp.Ole)
  | Bop_greater ->
    build_arithmetic (build_icmp Icmp.Sgt) (build_fcmp Fcmp.Ogt)
  | Bop_geq ->
    build_arithmetic (build_icmp Icmp.Sge) (build_fcmp Fcmp.Oge)
