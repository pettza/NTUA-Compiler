val lexer : Lexing.lexbuf -> Parser.token

exception Lexical_error of string * string * int * int