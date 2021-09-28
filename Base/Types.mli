(** Types for the pcl types and routines *)

open Identifier


type pcl_type =
  | Typ_int
  | Typ_real
  | Typ_bool
  | Typ_char
  | Typ_array of int option * pcl_type
  | Typ_pointer of pcl_type option
(** Type of pcl types *)


val is_pointer : pcl_type -> bool


val is_array : pcl_type -> bool


type formal =
  | F_byval of id * pcl_type
  | F_byref of id * pcl_type
(** Type of formal arguments *)


type proc_type = formal list
(** Type of procedures *)


type func_type = formal list * pcl_type
(** Type of functions *)

