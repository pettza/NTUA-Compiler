open Identifier
open Types
open Operators
open Symtbl


(* Symbol table containing the library routines *)
let library_symtbl =
  empty
  (* Output *)
  |> add "writeInteger" @@ Procedure ([F_byval ("n", Typ_int)], { def=true })
  |> add "writeBoolean" @@ Procedure ([F_byval ("b", Typ_bool)], { def=true })
  |> add "writeChar" @@ Procedure ([F_byval ("c", Typ_char)], { def=true })
  |> add "writeReal" @@ Procedure ([F_byval ("r", Typ_real)], { def=true })
  |> add "writeString" @@ Procedure ([F_byref ("s", Typ_array (None, Typ_char))], { def=true })
  (* Input *)
  |> add "readInteger" @@ Function (([], Typ_int), { def=true })
  |> add "readBoolean" @@ Function (([], Typ_bool), { def=true })
  |> add "readChar" @@ Function (([], Typ_char), { def=true })
  |> add "readReal" @@ Function (([], Typ_real), { def=true })
  |> add "readString" @@ Procedure ([F_byval ("size", Typ_int); F_byref ("s", Typ_array (None, Typ_char))],
                                    { def=true })
  (* Math functions *)
  |> add "abs" @@ Function (([F_byval ("n", Typ_int)], Typ_int), { def=true })
  |> add "fasb" @@ Function (([F_byval ("r", Typ_real)], Typ_real), { def=true })
  |> add "sqrt" @@ Function (([F_byval ("r", Typ_real)], Typ_real), { def=true })
  |> add "sin" @@ Function (([F_byval ("r", Typ_real)], Typ_real), { def=true })
  |> add "cos" @@ Function (([F_byval ("r", Typ_real)], Typ_real), { def=true })
  |> add "tan" @@ Function (([F_byval ("r", Typ_real)], Typ_real), { def=true })
  |> add "arctan" @@ Function (([F_byval ("r", Typ_real)], Typ_real), { def=true })
  |> add "exp" @@ Function (([F_byval ("r", Typ_real)], Typ_real), { def=true })
  |> add "ln" @@ Function (([F_byval ("r", Typ_real)], Typ_real), { def=true })
  |> add "pi" @@ Function (([], Typ_real), { def=true })
  (* Casting funtions *)
  |> add "trunc" @@ Function (([F_byval ("r", Typ_real)], Typ_int), { def=true })
  |> add "round" @@ Function (([F_byval ("r", Typ_real)], Typ_int), { def=true })
  |> add "ord" @@ Function (([F_byval ("c", Typ_char)], Typ_int), { def=true })
  |> add "chr" @@ Function (([F_byval ("n", Typ_int)], Typ_char), { def=true })


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


let type_of_sem_expr = function SemE_lvalue (_, pcl_type) | SemE_rvalue (_, pcl_type) -> pcl_type
