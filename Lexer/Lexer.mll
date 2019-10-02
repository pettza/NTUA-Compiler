 {
  open Parser

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum
    }
}

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let small_letter = ['a'-'z']
let big_letter = ['A'-'Z']
let id = (small_letter | big_letter) (small_letter | big_letter | digit | '_')*
let white = [' ' '\t' '\r' '\n']
let esc = '\\'['n' 't' 'r' '0' '\\' '\"' '\'']
let char = [^'\\' '\"' '\''] | esc

rule lexer = parse
| '.' { T_dot }
| digit+ as inum { T_int_const (int_of_string inum) }
| digit* frac? exp? as fnum { T_real_const (float_of_string fnum) }
| "\'"char"\'" as c { T_char_const (String.get c 1) }
| "\""char*"\"" as str { T_string_literal (String.sub str 1 (String.length str - 2)) }
| "do" { T_do }
| "while" { T_while }
| "if" { T_if }
| "then" { T_then }
| "else" { T_else }
| "goto" { T_goto }
| "label" { T_label }
| "forward" { T_forward }
| "function" { T_function }
| "procedure" { T_procedure }
| "program" { T_program }
| "var" { T_var }
| "result" { T_result }
| "return" { T_return }
| "array" { T_array }
| "of" { T_of }
| "integer" { T_integer }
| "real" { T_real }
| "char" { T_char }
| "boolean" { T_boolean }
| "and" { T_and }
| "not" { T_not }
| "or" { T_or }
| "true" { T_true }
| "false" { T_false }
| "begin" { T_begin }
| "end" { T_end }
| "new" { T_new }
| "nil" { T_nil }
| "dispose" { T_dispose }
| '=' { T_eq }
| '>' { T_less }
| '<' { T_greater }
| "<>" { T_neq }
| ">=" { T_geq }
| "<=" { T_leq }
| '+' { T_plus }
| '-' { T_minus }
| '*' { T_times }
| '/' { T_rdiv }
| "div" { T_div }
| "mod" { T_mod }
| '^' { T_deref }
| '@' { T_ref }
| ":=" { T_assign }
| ';' { T_semicolon }
| ':' { T_colon }
| '(' { T_lparen }
| ')' { T_rparen }
| ',' { T_comma }
| '[' { T_lbrack }
| ']' { T_rbrack }
| id as id { T_id id }
| white+ { lexer lexbuf }
| "(*" { comment lexbuf }
| _ as c { Printf.eprintf "malakia egrapses panika: '%c' (ascii: %d)\n" c (Char.code c);
           lexer lexbuf }
| eof { T_eof }
and comment = parse
| "*)" { lexer lexbuf }
| _ { comment lexbuf }
