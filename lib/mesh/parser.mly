%{
  open Syntax
%}

%token EQUALS

%token SEMICOLON
%token LBRACK RBRACK
%token LPAREN RPAREN
%token COMMA

%token LET

// Literals
%token <bool>   BOOL
%token <int>    INT
%token <float>  FLOAT
%token <string> STRING

%token <string> VAR

%token EOF

%start <Syntax.toplevel_cmd> toplevel
%start <Syntax.toplevel_cmd list> file

%%

file:
  | EOF;                                      { [] }
  | binding = lettop;                         { binding }
  | e = exprtop;                              { e }

lettop:
  | binding = let_binding; SEMICOLON; rest = file;      { binding :: rest }

exprtop:
  | e = expr; SEMICOLON; rest = file                    { Expr e :: rest }

toplevel:
  | e = expr; EOF                                       { Expr e }
  | binding = let_binding; EOF                          { binding }

let_binding: LET; varname = VAR; EQUALS; e = expr;   { Let (varname, e) }

expr:
  | varname = VAR;                                                 { EVar varname }
  | lit = literal;                                                 { ELit lit }
  | l = delimited(LBRACK, separated_list(COMMA, expr), RBRACK)  { EList l }
  | t = delimited(LPAREN, separated_list(COMMA, expr), RPAREN)     { ETuple t }

literal:
  | v = BOOL;     { Bool v }
  | v = INT;      { Int v }
  | v = FLOAT;    { Float v }
  | v = STRING;   { String v }
