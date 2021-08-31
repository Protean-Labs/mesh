%{
  open Syntax

  let fold_fun e args = 
    match args with
    | PTuple args -> List.fold_right (fun arg acc -> EFun (arg, acc)) args e
    | arg -> EFun (arg, e)

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

%nonassoc EQUALS
%nonassoc ARROW

// %start <Syntax.expr> expr
%start <Syntax.expr list> file

%%

file:
  | EOF                                                 { [] }
  | e = expr SEMICOLON rest = file                      { e :: rest }

expr:
  | e = op_expr                                                     { e }
  | args = pattern ARROW e = expr                                   { fold_fun e args }
  | LET p = pattern EQUALS e = expr                                 { ELet (p, e) }
  | varname = VAR                                                   { EVar varname }
  | lit = literal                                                   { ELit lit }
  | LPAREN t = separated_nonempty_list(COMMA, expr) RPAREN          { ETuple t }
  | LBRACK l = separated_list(COMMA, expr) RBRACK                   { EList l }

op_expr:
  | op = OPERATOR e = expr                                           { EApp (EVar op, e) }
  | e1 = expr op = OPERATOR e2 = expr                                { EApp (EApp (EVar op, e1), e2) }

pattern:
  | UNDERSCORE                                                      { PAny }
  | varname = VAR                                                   { PVar varname }
  | lit = literal                                                   { PLit lit }
  | LPAREN t = separated_nonempty_list(COMMA, pattern) RPAREN       { PTuple t }

literal:
  | v = BOOL     { Bool v }
  | v = INT      { Int v }
  | v = FLOAT    { Float v }
  | v = STRING   { String v }
  | UNIT         { Unit }
