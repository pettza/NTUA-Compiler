let cin =
  if Array.length Sys.argv > 1
  then open_in Sys.argv.(1)
  else stdin
in
let fname =
  if Array.length Sys.argv > 1
  then
    match String.split_on_char '.' Sys.argv.(1) with
    | name::_ -> name
    | [] -> failwith "This should be unreachable. Whoopsie"
  else "out"
in
let lexbuf = Lexing.from_channel cin in
try
  let ast = Parser.program Lexer.lexer lexbuf in
  Ast_typing.typecheck_ast ast;
  Ast_print.print_ast ast;
  let the_module = Codegen.codegen ast in
  Llvm.print_module (fname ^ ".ll") the_module;
with Parsing.Parse_error | Parser.Error -> 
  Printf.eprintf "syntax error: %s\n" (Lexing.lexeme lexbuf);
  exit 1
| Ast_typing.TypingError msg ->
  Printf.eprintf "%s\n" msg;
  exit 1