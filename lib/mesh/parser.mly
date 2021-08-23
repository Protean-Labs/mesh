%{
  open Syntax
%}

%token EQUALS

%token SEMICOLON

%token LET

// Literals
%token <bool>   BOOL
%token <int>    INT
%token <float>  FLOAT
%token <string> STRING

%token <string> VAR

%token EOF

%start <Syntax.expr> expr_eof
%%

expr_eof:
  | e = expr; EOF;    { e }

expr:
  | LET; varname = VAR; EQUALS; e = expr; SEMICOLON; rest = expr;   { Let (varname, e, rest) }
  | e = simple_expr;                                                { e }

simple_expr:
  | varname = VAR;                                                  { Var varname }
  | lit = literal;                                                  { Lit lit }

literal:
  | v = BOOL;     { Bool v }
  | v = INT;      { Int v }
  | v = FLOAT;    { Float v }
  | v = STRING;   { String v }
