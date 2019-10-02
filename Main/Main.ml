open Parser
open Lexer

let main =
  let cin =
    if Array.length Sys.argv > 1
    then open_in Sys.argv.(1)
    else stdin
  in
  let lexbuf = Lexing.from_channel cin in
  try
    let () = program lexer lexbuf in
    exit 0
  with Parsing.Parse_error
  | Error -> 
    Printf.eprintf "syntax error: %s\n" (Lexing.lexeme lexbuf);
    exit 1
