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
%token <string> T_const
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

%left T_plus T_minus
%left T_times

%start <unit> program

%%

program : T_program {}
