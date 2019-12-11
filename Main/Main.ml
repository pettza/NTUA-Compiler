let cin =
  if Array.length Sys.argv > 1
  then open_in Sys.argv.(1)
  else stdin
in
let lexbuf = Lexing.from_channel cin in
let ast =
  try
    (Parser.program Lexer.lexer lexbuf)
  with Parsing.Parse_error | Parser.Error -> 
    Printf.eprintf "syntax error: %s\n" (Lexing.lexeme lexbuf);
    exit 1
in
try
  Ast_typing.typecheck_ast ast;
  Ast_print.print_ast ast
with Ast_typing.TypingError msg ->
  Printf.eprintf "%s\n" msg;
  exit 1