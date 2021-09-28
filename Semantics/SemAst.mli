open Identifier
open Types
open Operators


val library_symtbl : Symtbl.sym_entry Symtbl.t
(** Symbol table containing the library routines *)


type sem_ast = { sem_prog_name : id; sem_body : sem_body }


and sem_body =
  {
    sem_vars : (id * pcl_type) list;
    sem_labels : id list;
    sem_routines : sem_routine list;
    sem_block : sem_block
  }


and sem_routine =
  | Sem_proc of id * proc_type * sem_body
  | Sem_func of id * func_type * sem_body


and sem_block = sem_stmt list


and sem_stmt =
  | SemSt_empty
  | SemSt_assign of sem_lvalue * sem_expr
  | SemSt_block of sem_block
  | SemSt_call of sem_call
  | SemSt_if of sem_expr * sem_stmt * sem_stmt option
  | SemSt_while of sem_expr * sem_stmt
  | SemSt_label of id * sem_stmt
  | SemSt_goto of id
  | SemSt_return
  | SemSt_new of sem_lvalue
  | SemSt_new_array of sem_expr * sem_lvalue
  | SemSt_dispose of sem_lvalue
  | SemSt_dispose_array of sem_lvalue


and sem_call = { sem_routine_name : id; sem_args : (sem_expr * formal) list; sem_type : pcl_type option }


and sem_expr =
  | SemE_lvalue of sem_lvalue * pcl_type
  | SemE_rvalue of sem_rvalue * pcl_type


and sem_lvalue =
  | SemLv_id of id
  | SemLv_result
  | SemLv_string of string
  | SemLv_array of (sem_lvalue * pcl_type) * sem_expr
  | SemLv_deref of sem_expr


and sem_rvalue =
  | SemRv_int of int
  | SemRv_bool of bool
  | SemRv_real of float
  | SemRv_char of char
  | SemRv_nil
  | SemRv_call of sem_call
  | SemRv_ref of sem_lvalue * pcl_type
  | SemRv_unop of unop * sem_expr
  | SemRv_binop of sem_expr * binop * sem_expr


val type_of_sem_expr : sem_expr -> pcl_type
