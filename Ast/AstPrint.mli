(** Printing utilities for the abstract syntax tree *)

open Ast
open Types
open Operators


val print_ast : ast -> unit
(** Prints the Ast *)


val string_of_type : pcl_type -> string


val string_of_unop : unop -> string


val string_of_binop : binop -> string


val string_of_ast_lvalue : ast_lvalue -> string


val string_of_ast_expr : ast_expr -> string