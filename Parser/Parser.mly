%{
  open Ast
  open Types
  open Operators
%}

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

(* Identidiers *)
%token <Identifier.id> T_id

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

(* End of file *)
%token T_eof

(* Priority and assosiativity *)
%nonassoc "=" "<" ">" "<=" ">=" "<>"
%left "+" "-" "or"
%left "*" "/" "div" "mod" "and"
%nonassoc "not"
%nonassoc "^"
%nonassoc "@"
%nonassoc "then"
%nonassoc "else"

(* Entry point *)
%start <Ast.ast> program

%%

program:
  | "program" prog_name = T_id ";" body = body "." T_eof
    { { prog_name; body } }


body:
  | decls = local* block = block  { { decls = List.concat decls; block } }


id_list:
  | l = separated_nonempty_list(",", T_id)  { l }


local:
  | "var" l = nonempty_list( l = id_list ":" t = pcl_type ";"  { List.map (fun id -> Loc_var (id, t)) l }) 
    { List.concat l }
  | "label" l = id_list ";"  { List.map (fun id -> Loc_label id) l }
  | h = header ";" b = body ";"  { [Loc_def (h, b)] }
  | "forward" h = header ";"  { [Loc_decl h] }


header:
  | "procedure" id = T_id "(" l = separated_list(";", formal) ")" 
    { H_proc (id, List.concat l) }
  | "function" id = T_id "(" l = separated_list(";", formal) ")" ":" t = pcl_noarray_type
    { H_func (id, (List.concat l, t)) }


formal:
  | "var" l = id_list ":" t = pcl_type  { List.map (fun id -> F_byref(id, t)) l }
  | l = id_list ":" t = pcl_noarray_type  { List.map (fun id -> F_byval(id, t)) l }


pcl_noarray_type:
  | "integer"  { Typ_int }
  | "real"  { Typ_real }
  | "boolean"  { Typ_bool }
  | "char"  { Typ_char }
  | "^" t = pcl_type  { Typ_pointer (Some t) }


pcl_complete_type:
  | t = pcl_noarray_type  { t }
  | "array" "[" size = T_int_const "]" "of" t = pcl_complete_type  { Typ_array (Some size, t) }


pcl_type:
  | t = pcl_complete_type  { t }
  | "array" "of" t = pcl_complete_type  { Typ_array (None, t) }


block:
  | "begin" l = separated_nonempty_list(";", stmt) "end"  { l }


stmt:
  |  { St_empty }
  | lv = l_value ":=" e = expr  { St_assign (lv, e) }
  | b = block  { St_block b }
  | c = call  { St_call c }
  | "if" e = expr "then" s = stmt  { St_if (e, s, None) }
  | "if" e = expr "then" s1 = stmt "else" s2 = stmt  { St_if (e, s1, Some s2) }
  | "while" e = expr "do" s = stmt  { St_while (e, s) }
  | id = T_id ":" s = stmt  { St_label (id, s) }
  | "goto" id = T_id  { St_goto id }
  | "return"  { St_return }
  | "new" e = option("[" e = expr "]" { e }) lv = l_value  { St_new (e, lv) }
  | "dispose" paren = option("[" "]" { () }) lv = l_value  { St_dispose (paren, lv) }


%inline expr: 
  | lv = l_value  { E_lvalue lv }
  | rv = r_value  { E_rvalue rv }


l_value:
  | id = T_id  { Lv_id id }
  | "result"  { Lv_result }
  | str = T_string_literal  { Lv_string str }
  | lv = l_value "[" e = expr "]"  { Lv_array (lv, e) }
  | e = expr "^"  { Lv_deref e }
  | "(" lv = l_value ")"  { lv }


r_value:
  | inum = T_int_const  { Rv_int inum }
  | "true"  { Rv_bool true }
  | "false"  { Rv_bool false }
  | fnum = T_real_const  { Rv_real fnum }
  | c = T_char_const  { Rv_char c }
  | "(" rv = r_value ")"  { rv }
  | "nil"  { Rv_nil }
  | c = call  { Rv_call c }
  | "@" lv = l_value  { Rv_ref lv }
  | unop = unop e = expr  { Rv_unop (unop, e) }
  | e1 = expr binop = binop e2 = expr  { Rv_binop (e1, binop, e2) }


call:
  | routine_name = T_id "(" args = separated_list(",", expr) ")"
    { { routine_name; args } }


%inline unop:
  | "not"  { Uop_not }
  | "+"  { Uop_plus }
  | "-"  { Uop_minus }


%inline binop:
  | "+"  { Bop_plus }
  | "-"  { Bop_minus }
  | "*"  { Bop_times }
  | "/"  { Bop_rdiv }
  | "div"  { Bop_div }
  | "mod"  { Bop_mod }
  | "or"  { Bop_or }
  | "and"  { Bop_and }
  | "="  { Bop_eq }
  | "<>"  { Bop_neq }
  | "<"  { Bop_less }
  | "<="  { Bop_leq }
  | ">"  { Bop_greater }
  | ">="  { Bop_geq }
