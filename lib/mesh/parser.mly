%{
  open Syntax

  let fold_fun e args = 
    List.fold_right (fun acc arg -> EFun (arg, acc)) args e

%}

%token EQUALS
%token ARROW

%token SEMICOLON
%token LBRACK RBRACK
%token LPAREN RPAREN
%token COMMA

%token LET

%token <string> OPERATOR
%left OPERATOR

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

let_binding: LET; varname = VAR; EQUALS; e = expr;      { Let (varname, e) }

expr:
  | varname = VAR;                                                  { EVar varname }
  | lit = literal;                                                  { ELit lit }
  | l = delimited(LBRACK, separated_list(COMMA, expr), RBRACK)      { EList l }
  | args = tuple; ARROW; e = expr;                                  { fold_fun e args }
  | t = tuple;                                                      { ETuple t }
  | op = OPERATOR; e = expr;                                        { EApp (EVar op, e) }
  | e1 = expr; op = OPERATOR; e2 = expr;                            { EApp (EApp (EVar op, e1), e2) }

tuple: t = delimited(LPAREN, separated_list(COMMA, expr), RPAREN)   { t }

literal:
  | v = BOOL;     { Bool v }
  | v = INT;      { Int v }
  | v = FLOAT;    { Float v }
  | v = STRING;   { String v }
