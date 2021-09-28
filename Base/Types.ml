open Identifier


type pcl_type =
  | Typ_int
  | Typ_real
  | Typ_bool
  | Typ_char
  | Typ_array of int option * pcl_type
  | Typ_pointer of pcl_type option (* Typ_pointer None is the type of nil *)


let is_pointer = function
  | Typ_pointer _ -> true
  | _ -> false


let is_array = function
  | Typ_array _ -> true
  | _ -> false


type formal =
  | F_byval of id * pcl_type
  | F_byref of id * pcl_type


type proc_type = formal list


type func_type = formal list * pcl_type
