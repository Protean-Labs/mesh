%{
  open Syntax
  open Syntax_util

  open Primitives

  open Parser_util
%}

// Literals
%token UNIT
%token <bool>   BOOL
%token <int>    INT
%token <float>  FLOAT
%token <string> STRING

%token <string> VAR
%token <string> MOD

%token <string> OPERATOR

%token LET
%token MODULE
%token ES6_FUN
%token EXTERNAL

%token SEMICOLON
%token COLON
%token LBRACK RBRACK
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COMMA
%token DOT

%token UNDERSCORE
%token EQUALS
%token ARROW
%token DOTDOTDOT

%token EOF

%left OPERATOR

%right EQUALS
// %right ARROW
%nonassoc UNIT
// %nonassoc ES6_FUN

// %start <Syntax.expr> expr
%start <Syntax.expr list> file

%on_error_reduce expr
%%

// ================================================================
// Helpers
// ================================================================
(* [lseparated_list(separator, X)] is same as [separated_list(separator, X)]
   except that it allows for trailing [seperator] token. *)

%public %inline lseparated_list(separator, X):
  xs = loption(lseparated_nonempty_list(separator, X))              { xs }

(* [lseparated_nonempty_list(separator, X)] is same as 
   [separated_nonempty_list(separator, X)] except that it allows for 
   trailing [seperator] token. *)

%public lseparated_nonempty_list(separator, X):
  | x = X                                                           { [ x ] }
  | x = X separator                                                 { [ x ] }
  | x = X separator xs = lseparated_nonempty_list(separator, X)     { x :: xs }

// ================================================================
// Expressions
// ================================================================
file:
  | EOF                                                     { [] }
  | e = expr SEMICOLON rest = file                          { e :: rest }

expr:
  | LET p = simple_pattern EQUALS e = expr                  
    { mk_expr ~loc:(mklocation $symbolstartpos $endpos) (ELet (p, e)) }
  
  | EXTERNAL p = simple_pattern EQUALS v = STRING           
    { mk_expr ~loc:(mklocation $symbolstartpos $endpos) (ELet (p, primitive_of_name v)) }
  
  | e = fun_def                                             { e }
  | e = fun_app                                             { e }
  | e = value_path                                          { e }
  | lit = literal                                           
    { mk_expr ~loc:(mklocation $symbolstartpos $endpos) (ELit lit) }
  
  | e = e_list                                              { e }
  | e = tuple                                               { e }
  | op = OPERATOR e = expr                                  
    { mk_expr ~loc:(mklocation $symbolstartpos $endpos) (EApp (mk_expr (EVar ([], op)), e)) }
  
  | e1 = expr op = OPERATOR e2 = expr                       
    { mk_expr ~loc:(mklocation $symbolstartpos $endpos) (EApp (mk_expr (EApp (mk_expr (EVar ([], op)), e1)), e2)) }
  
  | MODULE modname = MOD EQUALS 
    LBRACE body = structure RBRACE                          
    { mk_expr ~loc:(mklocation $symbolstartpos $endpos) (EMod (modname, body)) }
  
  | ES6_FUN p = simple_pattern ARROW e = fun_body           
    { mk_expr ~loc:(mklocation $symbolstartpos $endpos) (EFun (p, e)) }

fun_def:
  | UNIT ARROW e = fun_body                                           
    { mk_expr ~loc:(mklocation $symbolstartpos $endpos) (EFun (mk_pat (PLit Unit), e)) }
  
  | ES6_FUN LPAREN p = separated_nonempty_list(COMMA, simple_pattern) 
    RPAREN ARROW e = fun_body                                         { fold_fun e p }

// fun_def:
//   | ES6_FUN p = simple_pattern ARROW e = fun_body           { EFun (p, e) }

fun_body:
  | e = expr                                                        { e }
  | LBRACE e = seq_expr RBRACE                                      { e }

fun_app:
  | e = expr UNIT                                                   
    { mk_expr ~loc:(mklocation $symbolstartpos $endpos) (EApp (e, mk_elit_unit ())) }
  
  | e = expr LPAREN args = separated_list(COMMA, expr) RPAREN       { fold_app e args }

/** Note: Due to the fact that we are reusing the `tuple` rule for both tuple expressions (i.e.: ETuple)
    as well as for function argument tuple patterns (i.e.: PTuple) we cannot immediately return the ETuple. 
    This is because the rules for formatting tuples vary depending on the use case. For example, 
    in `let t = ((a, b));`, the double parantheses should be ignored (`t` is a simple 2-tuple) whereas
    in `let f = ((a, b)) => a;`, the double parantheses are actually important as they indicate that the 
    argument is a tuple (instead of two seperate arguments). Therefore, the `tuple` grammar rule only returns 
    the list of expressions `t`, which is transformed according to the parent rule. */
tuple: 
  | LPAREN t = separated_nonempty_list(COMMA, expr) RPAREN          
    { fmt_tuple t (mklocation $symbolstartpos $endpos) }

e_list:
  | LBRACK l = lseparated_list(COMMA, expr) DOTDOTDOT e = expr RBRACK  
    { fold_cons l e (mklocation $symbolstartpos $endpos) }
  
  | LBRACK l = lseparated_list(COMMA, expr) RBRACK                           
    { mk_expr ~loc:(mklocation $symbolstartpos $endpos) (EList l) }

seq_expr:
  | e = seq_expr_no_seq                                             { e }
  | e = expr SEMICOLON rest = seq_expr                              
    { mk_expr ~loc:(mklocation $symbolstartpos $endpos) (ESeq (e, rest)) }

seq_expr_no_seq:
  | e = expr SEMICOLON?                                             { e }

literal:
  | v = BOOL     { Bool v }
  | v = INT      { Int v }
  | v = FLOAT    { Float v }
  | v = STRING   { String v }
  | UNIT         { Unit }

value_path:
  | varname = VAR                                                   
    { mk_expr ~loc:(mklocation $symbolstartpos $endpos) (EVar ([], varname)) }

  | modname = MOD DOT vpath = value_path                            
    { fmt_value_path vpath modname (mklocation $symbolstartpos $endpos) }

// ================================================================
// Modules
// ================================================================
structure:
  | (* Empty *)                             { [] }
  | e = expr                                { [e] }
  | e = expr SEMICOLON rest = structure     { e :: rest }
;

// ================================================================
// Patterns
// ================================================================
simple_pattern:
  | p = simple_pattern_ident                                        { p }
  | p = simple_pattern_not_ident                                    { p }

simple_pattern_ident:
  | varname = VAR                                                   
    { mk_pat ~loc:(mklocation $symbolstartpos $endpos) (PVar varname) }

simple_pattern_not_ident:
  | UNDERSCORE                                                          
    { mk_pat ~loc:(mklocation $symbolstartpos $endpos) (PAny) }
  
  | lit = literal                                                       
    { mk_pat ~loc:(mklocation $symbolstartpos $endpos) (PLit lit) }

  | LPAREN t = separated_nonempty_list(COMMA, simple_pattern) RPAREN    
    { if List.length t == 1 
      then List.hd t 
      else mk_pat ~loc:(mklocation $symbolstartpos $endpos) (PTuple t) }
