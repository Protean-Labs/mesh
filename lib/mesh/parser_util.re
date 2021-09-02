open Syntax

exception InvalidPattern(string);

/** [pattern_of_expr(e)] returns a pattern [p] that matches the expression. 
    Used to convert "tuples" to function argument patterns. Raises 
    [InvalidPattern] if the epxression does not have a pattern equivalent. */
let rec pattern_of_expr = fun
  | ELit(lit)      => PLit(lit)
  | EVar(name)     => PVar(name)
  | ETuple(exprs)  => PTuple(List.map(pattern_of_expr, exprs))
  | e              => raise(InvalidPattern(string_of_expr(0, e)))
;

let fmt_fun_pattern = fun
  | ETuple(l)   => List.map(pattern_of_expr, l)
  | _           => raise(InvalidPattern("function argument pattern is not a tuple"))
;

/** [fold_fun(e, args)] returns a new value [e = EFun(...)] of type {expr} containing
    nested functions (one for each argument). E.g.: [fold_fun(e, [PVar("a"), PVar("b")])] 
    would return the following AST subtree: 
    
    (EFun PVar a =>
      (EFun PVar b =>
        e)) */
let fold_fun = (e, args) => 
  List.fold_right((arg, acc) => EFun(arg, acc), args, e);

let fold_app = (e, args) =>
  List.fold_left((acc, arg) => EApp(acc, arg), e, args);