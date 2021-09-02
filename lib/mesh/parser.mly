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

%token EOF

%left OPERATOR

%right EQUALS
%right ARROW
%nonassoc UNIT

// %start <Syntax.expr> expr
%start <Syntax.expr list> file

%%

file:
  | EOF                                                 { [] }
  | e = expr SEMICOLON rest = file                      { e :: rest }

expr:
  | e = fun_def                                                     { e }
  | e = fun_app                                                     { e }
  | LET p = simple_pattern EQUALS e = expr                          { ELet (p, e) }
  | varname = VAR                                                   { EVar varname }
  | lit = literal                                                   { ELit lit }
  | t = tuple                                                       { t }
  | LBRACK l = separated_list(COMMA, expr) RBRACK                   { EList l }
  | op = OPERATOR e = expr                                          { EApp (EVar op, e) }
  | e1 = expr op = OPERATOR e2 = expr                               { EApp (EApp (EVar op, e1), e2) }

fun_def:
  | LPAREN UNDERSCORE RPAREN ARROW e = expr                         
  | UNDERSCORE ARROW e = expr                                       { EFun (PAny, e) }
  | UNIT ARROW e = expr                                             { EFun (PLit Unit, e) }
  | varname = VAR ARROW e = expr                                    { EFun (PVar varname, e) }
  | args = tuple ARROW e = expr                                     { fold_fun e (fmt_fun_pattern args) }

fun_app:
  | e = expr UNIT                                                  { EApp (e, unit_lit ()) }
  | e = expr LPAREN args = separated_list(COMMA, expr) RPAREN      { fold_app (e) args }

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
