(** Unary and binary operator types *)

type unop =
  | Uop_not
  | Uop_plus
  | Uop_minus
(** Unary operator type *)


type binop =
  | Bop_plus
  | Bop_minus
  | Bop_times
  | Bop_rdiv
  | Bop_div
  | Bop_mod
  | Bop_or
  | Bop_and
  | Bop_eq
  | Bop_neq
  | Bop_less
  | Bop_leq
  | Bop_greater
  | Bop_geq
(** Binary operator type *)