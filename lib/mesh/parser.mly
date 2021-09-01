%{
  open Syntax

  open Easy_logging
  let logger = Logging.make_logger "Parser" Debug [Cli Debug]

  exception InvalidPattern of string

  let rec pattern_of_expr = function
    | ELit lit      -> PLit lit
    | EVar name     -> PVar name
    | ETuple exprs  -> PTuple (List.map pattern_of_expr exprs)
    | e             -> raise (InvalidPattern (string_of_expr 0 e))

  let fmt_fun_pattern = function
    | ETuple l    -> List.map pattern_of_expr l
    | _           -> raise (InvalidPattern "pattern is not a tuple")

  let fold_fun e args = 
    List.fold_right (fun arg acc -> EFun (arg, acc)) args e
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

// %start <Syntax.expr> expr
%start <Syntax.expr list> file

%%

file:
  | EOF                                                 { [] }
  | e = expr SEMICOLON rest = file                      { e :: rest }

expr:
  | e = fun_def                                                     { e }
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

tuple: LPAREN t = separated_nonempty_list(COMMA, expr) RPAREN       { ETuple t }

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
  | UNDERSCORE                                                          { logger#debug "PAny"; PAny }
  | lit = literal                                                       { logger#debug "PLit"; PLit lit }
  | LPAREN t = separated_nonempty_list(COMMA, simple_pattern) RPAREN    { logger#debug "PTuple"; if List.length t == 1 then List.hd t else PTuple t }
