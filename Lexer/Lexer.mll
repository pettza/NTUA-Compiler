 {
  open Parser

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum
    }

  let char_for_backslash = function
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

  exception Lexical_error of string * string * int * int
  
  let raise_lexical_error lexbuf msg =
    let p = Lexing.lexeme_start_p lexbuf in
    raise (Lexical_error (msg,
                          p.Lexing.pos_fname,
                          p.Lexing.pos_lnum,
                          p.Lexing.pos_cnum - p.Lexing.pos_bol + 1))
}

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let small_letter = ['a'-'z']
let big_letter = ['A'-'Z']
let id = (small_letter | big_letter) (small_letter | big_letter | digit | '_')*
let white = [' ' '\t']
let newline = ['\r' '\n']
let esc = '\\'['n' 't' 'r' '0' '\\' '\"' '\'']

rule lexer = parse
| '.' { T_dot }
| digit+ as inum { T_int_const (int_of_string inum) }
| digit* frac? exp? as fnum { T_real_const (float_of_string fnum) }
| '\'' [^ '\\'] '\'' { T_char_const (Lexing.lexeme_char lexbuf 1) }
| '\'' esc '\'' { T_char_const (char_for_backslash @@ Lexing.lexeme_char lexbuf 2)  }
| '\'' '\\' (_ as c)
    { raise_lexical_error lexbuf
        (Printf.sprintf "Illegal escape sequence \\%c" c)
    }
| '\"' { T_string_literal (string "" lexbuf) }
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
| newline { incr_linenum lexbuf; lexer lexbuf }
| "(*" { comment lexbuf }
| _ as c
  { raise_lexical_error lexbuf
      (Printf.sprintf "Illigal character: '%c' (ascii: %d)" c (Char.code c));
  }
| eof { T_eof }

and string acc = parse
| '\"' { acc }
| newline
  { raise_lexical_error lexbuf
      (Printf.sprintf "newline charecter inside string literal")
  }
| esc 
  { let c = Char.escaped @@ char_for_backslash @@ Lexing.lexeme_char lexbuf 1 in
    string (acc ^ c) lexbuf
  }
| '\\' (_ as c)
  { raise_lexical_error lexbuf
      (Printf.sprintf "illegal escape sequence \\%c" c)
  }
| _ as c { string (acc ^ (Char.escaped c)) lexbuf }

and comment = parse
| "*)" { lexer lexbuf }
| newline { incr_linenum lexbuf; comment lexbuf }
| _ { comment lexbuf }
