open Ast


exception TypingError of string


val typecheck_ast : ast -> unit