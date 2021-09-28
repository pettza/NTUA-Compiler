open Identifier
open Types


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
