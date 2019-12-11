open Llvm
open Symtbl
open Ast

let context = global_context ()
let the_module = create_module context "llvm program"
let builder = builder context
let void_type = void_type context
let function_names : (string, unit) Hashtbl.t = Hashtbl.create 10  

let function_name_gen id =
  let name = string_of_id id in
  let rec try_add name =
  match Hashtbl.find_opt function_names name with
  | None -> Hashtbl.add function_names name (); name
  | Some () -> try_add @@ name ^ "$"
  in
  try_add name


(* let real_type = double *)
let rec lltype_of_pcl_type = function
  | Typ_int -> i64_type context
  | Typ_real -> x86fp80_type context
  | Typ_bool -> i8_type context
  | Typ_char -> i8_type context
  | Typ_pointer pcl_type ->
    pointer_type @@ lltype_of_pcl_type pcl_type
  | Typ_array (Some n, pcl_type) ->
    array_type (lltype_of_pcl_type pcl_type) n
  | Typ_array (None, pcl_type) ->
    pointer_type @@ lltype_of_pcl_type pcl_type


let rec codegen { prog_name= _; body } = codegen_body body ~symtbl:empty


and codegen_body { decls; block } ~symtbl =
  List.fold_left codegen_local ~symtbl decls;
  codegen_block block ~symtbl


and codegen_local ~symtbl = function
  | Loc_var vars ->
    let build_id symtbl (id, pcl_type) =
      let alloca_val = build_alloca (lltype_of_pcl_type pcl_type) (string_of_id id) builder in
      add id alloca_val symtbl
    in
    List.fold_left build_id symtbl vars
  | Loc_label _ | Loc_decl _ -> symtbl
  | Loc_def (header, body) ->
    let arraify_formal_types formals =
      Array.of_list
      @@
      List.map
        (function | F_byref (_, pcl_type) 
                  | F_byval (_, pcl_type) -> lltype_of_pcl_type pcl_type)
        formals
    in
    let id =
      match header with
      | H_func (id, _) | H_proc (id, _) -> id 
    in
    let header_type =
      match header with
      | H_func (_, (formals, pcl_type)) ->
        function_type (lltype_of_pcl_type pcl_type) (arraify_formal_types formals)
      | H_proc (_, formals) ->
        function_type void_type (arraify_formal_types formals)
    in
    let f_val = define_function (function_name_gen id) header_type the_module in
