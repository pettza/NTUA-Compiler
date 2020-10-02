(** Symbol table and related types.
A symbol table is a map with keys of type id *)


module Id : Set.OrderedType with type t = string


type id = Id.t
(** Identifier type *)


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


type def_record = { def:bool }
(** Type of record for sym entry of routines,
in order to keep track of whether they have been implemented *)


type sym_entry =
  | Program
  | Label of { mutable used:bool } (* The field used makes sure a label is not used more the once *)
  | Variable of pcl_type
  | Procedure of proc_type * def_record
  | Function of func_type * def_record
(** Type of symbol table entries for type checking *)


include Map.S with type key := id


val add_tbl : 'a t -> 'a t -> 'a t
(** Union of two tables, keeping the entries of the second table
in case they have entries for the same id *)

val add_list : id list -> 'a -> 'a t -> 'a t
(** Adds a list of ids with same entries to the table *)
