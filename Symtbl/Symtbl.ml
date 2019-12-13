module Id : Set.OrderedType with type t = string = String


type id = Id.t


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
  | F_byval of id * pcl_type
  | F_byref of id * pcl_type


type proc_type = formal list


type func_type = formal list * pcl_type


type def_record = { def:bool }


type sym_entry =
  | Program
  | Label of { mutable used:bool }
  | Variable of pcl_type
  | Procedure of proc_type * def_record
  | Function of func_type * def_record


open Map
module Symtbl : S with type key := id = Make(Id)


include Symtbl


let add_tbl symtbl1 symtbl2 =
  union (fun _ _ x -> Some x) symtbl1 symtbl2


let add_list id_list sym_type symtbl =
  List.fold_left (fun symtbl id -> add id sym_type symtbl) symtbl id_list
