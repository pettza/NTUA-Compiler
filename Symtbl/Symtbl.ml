module Id : Set.OrderedType with type t = string = String


type id = Id.t


let id_of_string : string -> id = Fun.id


let string_of_id : id -> string = Fun.id


type pcl_type =
  | Typ_int
  | Typ_real
  | Typ_bool
  | Typ_char
  | Typ_array of int option * pcl_type
  | Typ_pointer of pcl_type


let is_pointer = function
  | Typ_pointer _ -> true
  | _ -> false


let is_array = function
  | Typ_array _ -> true
  | _ -> false


type formal =
  | F_byval of id list * pcl_type
  | F_byref of id list * pcl_type


type proc_type = formal list


type func_type = formal list * pcl_type


type sym_entry =
  | Program
  | Label
  | Variable of pcl_type
  | Procedure of proc_type
  | Function of func_type


open Map
module Symtbl : S with type key := id = Make(Id)


include Symtbl


let add_list id_list sym_type symtbl =
  List.fold_left (fun symtbl id -> add id sym_type symtbl) symtbl id_list
