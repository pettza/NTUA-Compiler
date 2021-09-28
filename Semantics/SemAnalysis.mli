open Ast
open SemAst


exception SemanticError of string

val typecheck_ast : ast -> sem_ast