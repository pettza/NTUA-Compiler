(* Flow control *)
%token T_do "do"
%token T_while "while"
%token T_if "if"
%token T_then "then"
%token T_else "else"
%token T_goto "goto"
%token T_label "label"

(* Function/Procedure/program related *)
%token T_forward "forward"
%token T_function "function"
%token T_procedure "procedure"
%token T_program "program"
%token T_var "var"
%token T_result "result"
%token T_return "return"

(* Variable declaration/assignment *)
%token T_array "array"
%token T_of "of"
%token T_integer "integer"
%token T_real "real"
%token T_char "char"
%token T_boolean "boolean"
%token T_assign ":="
%token <int> T_int_const
%token <float> T_real_const
%token <char> T_char_const
%token <string> T_string_literal

%token <string> T_id

(* Boolean stuff *)
%token T_and "and"
%token T_not "not"
%token T_or "or"
%token T_true "true"
%token T_false "false"

(* Arithmetic operators *)
%token T_eq "="
%token T_less "<"
%token T_greater ">"
%token T_neq "<>"
%token T_leq "<="
%token T_geq ">="
%token T_plus "+"
%token T_minus "-"
%token T_times "*"
%token T_rdiv "/"
%token T_div "div"
%token T_mod "mod"

(* Pointer stuff *)
%token T_new "new"
%token T_dispose "dispose"
%token T_nil "nil"
%token T_deref "^"
%token T_ref "@"

(* Seperators *)
%token T_begin "begin"
%token T_end "end"
%token T_semicolon ";"
%token T_dot "."
%token T_colon ":"
%token T_comma ","
%token T_lparen "("
%token T_rparen ")"
%token T_lbrack "["
%token T_rbrack "]"

%token T_eof

%nonassoc "=" "<" ">" "<=" ">=" "<>"
%left "+" "-" "or"
%left "*" "/" "div" "mod" "and"
%nonassoc "not"
(* unary ops *)
%nonassoc "^"
%nonassoc "@"
%nonassoc "then"
%nonassoc "else"

%start <unit> program

%%

program:
  | "program" T_id ";" body "." T_eof   { print_string "program"; print_newline () }

body:
  | local* block  { print_string "body"; print_newline () }

local:
  | "var" nonempty_list(separated_nonempty_list(",", T_id) ":" pcl_type ";" {})  { print_string "local"; print_newline () }
  | "label" separated_nonempty_list(",", T_id) ";"  { print_string "local"; print_newline () }
  | header ";" body ";"  { print_string "local"; print_newline () }
  | "forward" header ";"  { print_string "local"; print_newline () }

header:
  | "procedure" T_id "(" separated_list(";", formal) ")"  { print_string "header"; print_newline () }
  | "function" T_id "(" separated_list(";", formal) ")" ":" pcl_type  { print_string "header"; print_newline () }

formal:
  | "var"? separated_nonempty_list(",", T_id) ":" pcl_type  { print_string "formal"; print_newline () }

pcl_type:
  | "integer"  { print_string "pcl_type"; print_newline () }
  | "real"  { print_string "pcl_type"; print_newline () }
  | "boolean"  { print_string "pcl_type"; print_newline () }
  | "char"  { print_string "pcl_type"; print_newline () }
  | "array" option("[" T_int_const "]" {}) "of" pcl_type  { print_string "pcl_type"; print_newline () }
  | "^" pcl_type  { print_string "pcl_type"; print_newline () }

block:
  | "begin" separated_nonempty_list(";", stmt) "end"  { print_string "block"; print_newline () }

stmt:
  |  { print_string "stmt"; print_newline () }
  | l_value ":=" expr  { print_string "stmt"; print_newline () }
  | block  { print_string "stmt"; print_newline () }
  | call  { print_string "stmt"; print_newline () }
  | "if" expr "then" stmt  { print_string "stmt"; print_newline () }
  | "if" expr "then" stmt "else" stmt  { print_string "stmt"; print_newline () }
  | "while" expr "do" stmt  { print_string "stmt"; print_newline () }
  | T_id ":" stmt  { print_string "stmt"; print_newline () }
  | "goto" T_id  { print_string "stmt"; print_newline () }
  | "return"  { print_string "stmt"; print_newline () }
  | "new" option("[" expr "]" {}) l_value  { print_string "stmt"; print_newline () }
  | "dispose" option("[" "]" {}) l_value  { print_string "stmt"; print_newline () }

%inline expr: 
  | l_value  { print_string "expr"; print_newline () }
  | r_value  { print_string "expr"; print_newline () }

l_value:
  | T_id  { print_string "l_value"; print_newline () }
  | "result"  { print_string "l_value"; print_newline () }
  | T_string_literal  { print_string "l_value"; print_newline () }
  | l_value "[" expr "]"  { print_string "l_value"; print_newline () }
  | expr "^"  { print_string "l_value"; print_newline () }
  | "(" l_value ")"  { print_string "l_value"; print_newline () }

r_value:
  | T_int_const  { print_string "r_value"; print_newline () }
  | "true"  { print_string "r_value"; print_newline () }
  | "false"  { print_string "r_value"; print_newline () }
  | T_real_const  { print_string "r_value"; print_newline () }
  | T_char_const  { print_string "r_value"; print_newline () }
  | "(" r_value ")"  { print_string "r_value"; print_newline () }
  | "nil"  { print_string "r_value"; print_newline () }
  | call  { print_string "r_value"; print_newline () }
  | "@" l_value  { print_string "r_value"; print_newline () }
  | unop expr  { print_string "r_value"; print_newline () }
  | expr binop expr  { print_string "r_value"; print_newline () }

call:
  | T_id "(" separated_list(",", expr) ")"  { print_string "call"; print_newline () }

%inline unop:
  | "not"  { print_string "unop"; print_newline () }
  | "+"  { print_string "unop"; print_newline () }
  | "-"  { print_string "unop"; print_newline () }

%inline binop:
  | "+"  { print_string "binop"; print_newline () }
  | "-"  { print_string "binop"; print_newline () }
  | "*"  { print_string "binop"; print_newline () }
  | "/"  { print_string "binop"; print_newline () }
  | "div"  { print_string "binop"; print_newline () }
  | "mod"  { print_string "binop"; print_newline () }
  | "or"  { print_string "binop"; print_newline () }
  | "and"  { print_string "binop"; print_newline () }
  | "="  { print_string "binop"; print_newline () }
  | "<>"  { print_string "binop"; print_newline () }
  | "<"  { print_string "binop"; print_newline () }
  | "<="  { print_string "binop"; print_newline () }
  | ">"  { print_string "binop"; print_newline () }
  | ">="  { print_string "binop"; print_newline () }
