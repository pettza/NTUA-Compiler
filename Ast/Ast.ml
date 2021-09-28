open Identifier
open Types
open Operators


type ast = { prog_name : id; body : ast_body }


and ast_body = { decls : ast_local list; block : ast_block }


and ast_local =
  | Loc_var of (id * pcl_type)
  | Loc_label of id
  | Loc_def of ast_header * ast_body
  | Loc_decl of ast_header


and ast_header =
  | H_proc of id * proc_type
  | H_func of id * func_type


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
  | St_dispose of unit option * ast_lvalue


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
