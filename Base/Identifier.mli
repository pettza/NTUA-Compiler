(** Identifier type *)

module Id : Set.OrderedType with type t = string


type id = Id.t
(** Identifier type *)


(* type pos = { lnum : int; cnum : int } *)
(** Position in file. Record with line and column number *)


(* type id_pos = { id : id; pos : pos } *)
(** Record that holds an id and a pos *)