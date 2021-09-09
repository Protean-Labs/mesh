%{
  open Syntax
  open Parser_util
%}

// Literals
%token UNIT
%token <bool>   BOOL
%token <int>    INT
%token <float>  FLOAT
%token <string> STRING

%token <string> VAR

%token <string> OPERATOR

%token LET

%token SEMICOLON
%token LBRACK RBRACK
%token LPAREN RPAREN
%token COMMA

%token UNDERSCORE
%token EQUALS
%token ARROW
%token DOTDOTDOT

%token EOF

%left OPERATOR

%right EQUALS
%right ARROW
%nonassoc UNIT

// %start <Syntax.expr> expr
%start <Syntax.expr list> file

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
  | EOF                                                 { [] }
  | e = expr SEMICOLON rest = file                      { e :: rest }

expr:
  | e = fun_def                                            { e }
  | e = fun_app                                            { e }
  | LET p = simple_pattern EQUALS e = expr                 { ELet (p, e) }
  | varname = VAR                                          { EVar varname }
  | lit = literal                                          { ELit lit }
  | e = e_list                                             { e }
  | e = tuple                                              { e }
  | op = OPERATOR e = expr                                 { EApp (EVar op, e) }
  | e1 = expr op = OPERATOR e2 = expr                      { EApp (EApp (EVar op, e1), e2) }

fun_def:
  | LPAREN UNDERSCORE RPAREN ARROW e = expr                         
  | UNDERSCORE ARROW e = expr                                       { EFun (PAny, e) }
  | UNIT ARROW e = expr                                             { EFun (PLit Unit, e) }
  | varname = VAR ARROW e = expr                                    { EFun (PVar varname, e) }
  | args = tuple ARROW e = expr                                     { fold_fun e (fmt_fun_pattern args) }

fun_app:
  | e = expr UNIT                                                  { EApp (e, unit_lit ()) }
  | e = expr LPAREN args = separated_list(COMMA, expr) RPAREN      { fold_app e args }

e_list:
  | LBRACK l = lseparated_list(COMMA, expr) DOTDOTDOT e = expr RBRACK  { fold_cons l e }
  | LBRACK l = lseparated_list(COMMA, expr) RBRACK                           { EList l }

tuple: LPAREN t = separated_nonempty_list(COMMA, expr) RPAREN      { ETuple t }

literal:
  | v = BOOL     { Bool v }
  | v = INT      { Int v }
  | v = FLOAT    { Float v }
  | v = STRING   { String v }
  | UNIT         { Unit }

// ================================================================
// Patterns
// ================================================================
simple_pattern:
  | p = simple_pattern_ident                                        { p }
  | p = simple_pattern_not_ident                                    { p }

simple_pattern_ident:
  | varname = VAR                                                   { PVar varname }

simple_pattern_not_ident:
  | UNDERSCORE                                                          { PAny }
  | lit = literal                                                       { PLit lit }
  | LPAREN t = separated_nonempty_list(COMMA, simple_pattern) RPAREN    { if List.length t == 1 then List.hd t else PTuple t }
