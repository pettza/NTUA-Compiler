(** Symbol table and related types.
A symbol table is a map with keys of type id and values of type sym_entry *)


module Id : Set.OrderedType


type id = Id.t
(** Identifier type *)


val id_of_string : string -> id


val string_of_id : id -> string


type pcl_type =
  | Typ_int
  | Typ_real
  | Typ_bool
  | Typ_char
  | Typ_array of int option * pcl_type
  | Typ_pointer of pcl_type
(** Type of pcl types *)


val is_pointer : pcl_type -> bool


val is_array : pcl_type -> bool


type formal =
  | F_byval of id list * pcl_type
  | F_byref of id list * pcl_type
(** Type of formal arguments.
They can be declared in groups of same type and evaluation strategy *)


type proc_type = formal list
(** Type of procedures *)


type func_type = formal list * pcl_type
(** Type of functions *)


type sym_entry =
  | Program
  | Label
  | Variable of pcl_type
  | Procedure of proc_type
  | Function of func_type
(** Type of symbol table entrys *)


include Map.S with type key := id


val add_list : id list -> 'a -> 'a t -> 'a t